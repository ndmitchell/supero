
module Optimise.All(optimise, termination, Termination) where

import Yhc.Core
import Control.Monad
import System.FilePath
import System.Directory
import System.IO
import Optimise.Evaluate
import Optimise.Generate
import General

-- just rexports
import Optimise.Termination(termination)
import Optimise.State(Termination)


optimise :: Termination -> FilePath -> FilePath -> IO Answer
optimise term src obj = do
    b <- doesFileExist "log.txt"
    when b $ removeFile "log.txt"
    h <- openFile "log.txt" WriteMode

    srcMain <- haskellFile (src </> "Main")
    let dest = obj </> "Main.yca"
    b <- recompile srcMain dest
    when b $
        system_ ("yhc " ++ srcMain ++ " --linkcore" ++
                 " --objdir=" ++ obj ++ " --hidir=" ++ obj)
                (obj </> "compileyhc.stdout")
                (obj </> "compileyhc.stderr")
    core <- loadCore (obj </> "Main.yca")
    over <- loadOverlay
    (cont,core) <- return $ liftMain $ coreOverlay core over
    core <- return $ coreReachable ["main"] $ transs $ coreReachable ["main"] core
    core <- evaluate h term (output obj) core
    hClose h
    when (not cont) $ error "Aborted as no main available"
    
    let exe = obj </> "main" ++ (if isWindows then ".exe" else "")
    generate (obj </> "Main_.hs") core
    system_ ("ghc --make " ++ (obj </> "Main_.hs") ++ " -O2 -fasm " ++
             " -odir " ++ obj ++ " -hidir " ++ obj ++ " -o " ++ exe)
            (obj </> "compileghc.stdout")
            (obj </> "compileghc.stderr")
    return Success
    

output obj n core = do
    let sn = obj </> show n
    saveCore  (sn ++ ".ycr") core
    writeFile (sn ++ "__.hs") $ show core
    generate  (sn ++ ".hs"  ) core
    writeFile (sn ++ ".html") $ coreHtml core
    putStrLn $ "Written file " ++ show n


loadOverlay :: IO Core
loadOverlay = do
    let input = "library/Overlay.hs"
        output = "library/Overlay.ycr"
    b <- doesFileExist output
    build <- if not b then return True else do
        i <- getModificationTime input
        o <- getModificationTime output
        return $ i > o
    when build $ system_ ("yhc " ++ input ++ "  --core")
        "library/Overlay.stdout" "library/Overlay.stderr"
    loadCore output


transs = ensureInvariants [ConsecutiveFuncs, NoCorePos, NoRecursiveLet, NoCaseDefaultOne]
       . transformExpr tweak


-- cannot be done by Overlay since case _ converts to nothing when compiled
tweak (CoreApp (CoreFun s) [x,y])
    | s == "Prelude;seq" || s == "SEQ" = CoreCase x [(PatDefault,y)]
tweak (CoreFun "Prelude;otherwise") = CoreCon "Prelude;True"
tweak x = x


-- return True if you can actually lift Main up
liftMain :: Core -> (Bool, Core)
liftMain core = (not small, applyFuncCore f core)
    where
        small = "Main;main_small" `elem` map coreFuncName (coreFuncs core)

        f (CoreFunc "main" [] x)
            | small = CoreFunc "main_" ["x"] (CoreVar "x")
            | otherwise = CoreFunc "main" ["real"] (CoreApp x [CoreVar "real"])
        f (CoreFunc "Main;main_small" args body) = CoreFunc "main" args body
        f x = x



module Optimise.All(optimise, termination) where

import Yhc.Core
import Control.Monad
import System.FilePath
import System.Directory
import Optimise.Evaluate
import Optimise.Generate
import Optimise.Termination
import General


optimise :: FilePath -> FilePath -> IO (Either String String)
optimise src obj = do
    b <- doesFileExist (obj </> "Main.yca")
    when (not b) $ do
        system_ $ "yhc " ++ (src </> "Main") ++ " --linkcore" ++
                  " --objdir=" ++ obj ++ " --hidir=" ++ obj ++
                  "  > " ++ (obj </> "compileyhc.stdout") ++
                  " 2> " ++ (obj </> "compileyhc.stderr")
    core <- loadCore (obj </> "Main.yca")
    over <- loadOverlay
    core <- return $ coreReachable ["main"] $ transs $ coreReachable ["main"] $ liftMain $ coreOverlay core over
    core <- evaluate (output obj) core
    
    let exe = obj </> "main.exe"
    generate (obj </> "Main_.hs") core
    system_ $ "ghc --make " ++ (obj </> "Main_.hs") ++ " -O2 " ++
              " -odir " ++ obj ++ " -hidir " ++ obj ++ " -o " ++ exe ++
              "  > " ++ (obj </> "compileghc.stdout") ++
              " 2> " ++ (obj </> "compileghc.stderr")
    return $ Right exe
    

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
    when build $ system_ $ "yhc " ++ input ++ "  --core"
    loadCore output


transs = ensureInvariants [ConsecutiveFuncs, NoCorePos, NoRecursiveLet, NoCaseDefaultOne]
       . transformExpr tweak


-- cannot be done by Overlay since case _ converts to nothing when compiled
tweak (CoreApp (CoreFun s) [x,y])
    | s == "Prelude;seq" || s == "SEQ" = CoreCase x [(PatDefault,y)]
tweak (CoreFun "Prelude;otherwise") = CoreCon "Prelude;True"
tweak x = x


liftMain = applyFuncCore f
    where
        f (CoreFunc "main" [] x) = CoreFunc "main" ["real"] (CoreApp x [CoreVar "real"])
        f x = x


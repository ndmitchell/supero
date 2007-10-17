
module Main where

import Yhc.Core hiding (collectAllVars)
import Control.Monad
import Generate
--import Firstify.Firstify
--import Report
import Safe
import CoreUtil
import Data.List
import Data.Char
import System.Directory
import System.Environment
import System.Cmd
import System.Exit
import System.Time
import System.IO
import Control.Arrow

import Evaluate5

main = do
    args <- getArgs
    let (numbers,names) = partition (all isDigit) args
    if null numbers then
        mapM_ optimise names
     else
        mapM_ (benchmark (read $ head numbers)) names


optimise :: FilePath -> IO ()
optimise file = do
    system_ $ "yhc library/Overlay.hs --core"
    system_ $ "yhc test/" ++ file ++ "/" ++ file ++ ".hs --linkcore --hide"
    core <- loadCore ("test/" ++ file ++ "/ycr/" ++ file ++ ".yca")
    over <- loadCore "library/Overlay.ycr"
    core <- return $ coreReachable ["main"] $ transs $ coreReachable ["main"] $ liftMain $ coreOverlay core over
    evaluate (output file) core
    return ()


data Mode = C | GHC | Supero
            deriving (Show,Ord,Eq)

show_ x = map toLower $ show x

showMode x = s ++ replicate (6 - length s) ' '
    where s = show x

showTime x = left2 ++ "." ++ right
    where
        s = show x
        ss = replicate (3 - length s) '0' ++ s
        (left,right) = splitAt (length ss - 2) ss
        left2 = replicate (2 - length left) ' ' ++ left

clockMilli (TOD a b) = (a * 100) + (b `div` 10000000000)


showPerc x = replicate (3 - length s) ' ' ++ s ++ "%"
    where s = show x


benchmark :: Int -> FilePath -> IO ()
benchmark i file = do
    settings <- readSettings
    settings <- return $ lookupJustDef [] "" settings ++
                         lookupJustDef [] file settings
    let obj = lookupJustDef "obj" "obj" settings ++ "/" ++ file

    -- first compile the Supero
    ensureDirectory $ obj ++ "/bin"
    ensureDirectory $ obj ++ "/supero"
    system_ $ "ghc -O2 --make -fasm test/" ++ file ++ "/4.hs " ++
              "-o " ++ obj ++ "/bin/supero_.exe " ++
              "-hidir " ++ obj ++ "/supero " ++
              "-odir " ++ obj ++ "/supero"

    -- then compile ghc, using file_ if it exists
    ensureDirectory $ obj ++ "/ghc"
    b <- doesFileExist $ "test/" ++ file ++ "/" ++ file ++ "_.hs"
    let haskell = "test/" ++ file ++ "/" ++ file ++ ['_'|b] ++ ".hs"
    system_ $ "ghc -O2 --make -fasm " ++ haskell ++ " " ++
              "-o " ++ obj ++ "/bin/ghc_.exe " ++
              "-hidir " ++ obj ++ "/ghc " ++
              "-odir " ++ obj ++ "/ghc"

    -- then compile C
    hasC <- doesFileExist $ "test/" ++ file ++ "/" ++ file ++ ".c"
    when hasC $ do
        ensureDirectory $ obj ++ "/c"
        system_ $ "ghc -optc-O3 test/" ++ file ++ "/" ++ file ++ ".c " ++
                  "-odir " ++ obj ++ "/c/../../ " ++
                  "-o " ++ obj ++ "/bin/c_.exe"

    -- now run the benchmarks, by default twice each
    let count x = replicate (i * read (lookupJustDef "2" ("repeat_" ++ show_ x) settings)) x
        todo = concat $ transpose [count GHC, if hasC then count C else [], count Supero]

    let pStdin = lookupJustDef "" "stdin" settings
        pTextfile = lookupJustDef defaultTextFile "textfile" settings
        pArgs = lookupJustDef "" "args" settings

    putStrLn $ "Benchmarking " ++ file
    res <- flip mapM todo $ \p -> do
        tBegin <- getClockTime
        system_ $
            (if pStdin == "textfile" then "type " ++ pTextfile ++ " | " else "") ++
            slashes obj ++ "\\bin\\" ++ show_ p ++ "_.exe " ++ pArgs ++
            " > output.txt"
        tEnd <- getClockTime
        let tTime = clockMilli tEnd - clockMilli tBegin
        output <- readFileStrict "output.txt"
        putStrLn $ showMode p ++ " = " ++ showTime tTime
        return (output,(p,tTime))

    let (outs,times) = unzip res
    when (length (nub outs) > 1) $ error $ "Outputs do not all match:\n" ++ show outs
    let summary = map minimum $ groupBy ((==) `on` fst) $ sort times
        ghcVal = lookupJust GHC summary
    putStrLn $ replicate 10 '-'
    putStrLn $ "Results for " ++ file
    putStr $ unlines [showMode p ++ " = " ++ showTime v ++ "  " ++ showPerc ((v * 100) `div` ghcVal)
                     | (p,v) <- summary]
    putStrLn $ replicate 10 '='
    putStrLn ""


defaultTextFile = "C:\\Windows\\WindowsUpdate.log"


slashes = map f
    where
        f '/' = '\\'
        f x = x


readSettings :: IO [(String,[(String,String)])]
readSettings = do
    liftM2 (++) (readSettingsFile "settings.txt")
                (readSettingsFile "test/settings.txt")


readSettingsFile :: FilePath -> IO [(String,[(String,String)])]
readSettingsFile file = do
    b <- doesFileExist file
    if not b then return [] else do
        src <- readFile file
        let lns = filter (not . null) (lines src) ++ [":"]
        return $ f [""] [] lns
    where
        f :: [String] -> [(String,String)] -> [String] -> [(String,[(String,String)])]
        f keys vals ((':':names):rest) = map (flip (,) vals) keys ++ f (words names) [] rest
        f keys vals (x:rest) = f keys ((key,val):vals) rest
            where (key:val:_) = words x
        f keys vals [] = []


output file n core = do
    let sn = show n
    saveCore  ("test/" ++ file ++ "/" ++ sn ++ ".ycr") core
    writeFile ("test/" ++ file ++ "/" ++ sn ++ "__.hs") $ show core
    generate  ("test/" ++ file ++ "/" ++ sn ++ ".hs"  ) core
    writeFile ("test/" ++ file ++ "/" ++ sn ++ ".html") $ coreHtml core
    putStrLn $ "Written file " ++ show n


transs = ensureInvariants [ConsecutiveFuncs, NoCorePos, NoRecursiveLet, NoCaseDefaultOne]
       . transformExpr removeSeq


-- cannot be done by Overlay since case _ converts to nothing when compiled
removeSeq (CoreApp (CoreFun s) [x,y])
    | s == "Prelude;seq" || s == "SEQ" = CoreCase x [(PatDefault,y)]
removeSeq x = x


liftMain = applyFuncCore f
    where
        f (CoreFunc "main" [] x) = CoreFunc "main" ["real"] (CoreApp x [CoreVar "real"])
        f x = x

ensureDirectory = createDirectoryIfMissing True

system_ cmd = do
    res <- system cmd
    when (res /= ExitSuccess) $ error $ "ERROR: System call failed\n" ++ cmd


readFileStrict :: FilePath -> IO String
readFileStrict file = do
    h <- openFile file ReadMode
    src <- hGetContents h
    () <- length src `seq` return ()
    hClose h
    return src

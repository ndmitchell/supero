
module Main where

import Yhc.Core hiding (collectAllVars)
import Control.Monad
import Generate
--import Firstify.Firstify
--import Report
import Data.List
import Data.Char
import System.Directory
import System.Environment
import System.Cmd
import System.Exit

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
    res <- system $ "yhc library/Overlay.hs --core"
    when (res /= ExitSuccess) $ error "Failed to compile overlay"
    res <- system $ "yhc test/" ++ file ++ "/" ++ file ++ ".hs --linkcore --hide"
    when (res /= ExitSuccess) $ error "Failed to compile"
    core <- loadCore ("test/" ++ file ++ "/ycr/" ++ file ++ ".yca")
    over <- loadCore "library/Overlay.ycr"
    core <- return $ coreReachable ["main"] $ transs $ coreReachable ["main"] $ liftMain $ coreOverlay core over
    evaluate (output file) core
    return ()


benchmark :: Int -> FilePath -> IO ()
benchmark i file = do
    settings <- readSettings
    return ()


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
        return $ f [":"] [] lns
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

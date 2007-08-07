
module Main where

import Yhc.Core hiding (collectAllVars)
import Control.Monad
import Generate
--import Firstify.Firstify
--import Report
import Data.List
import System.Directory
import System.Environment
import System.Cmd

import Evaluate

main = do
    [file] <- getArgs
    system $ "yhc test/" ++ file ++ "/" ++ file ++ ".hs --linkcore --hide"
    core <- loadCore ("test/" ++ file ++ "/ycr/" ++ file ++ ".yca")
    over <- loadCore "library/Overlay.ycr"
    core <- return $ transs $ coreReachable ["main"] $ liftMain $ coreOverlay core over
    output file 1 core
    
    output file 4 (evaluate core)

{-
    error "done"

    putStrLn "Firstifying basic"
    core <- return $ firstify core
    output file 2 core
    putStrLn $ unlines $ report core

    core <- return $ firstifyDataPrepare core
    output file 3 core

    putStrLn "Firstifying scary"
    core <- return $ firstifyData core
    output file 4 core
    putStrLn $ unlines $ report core
-}

output file n core = do
    let sn = show n
    writeFile ("test/" ++ file ++ "/" ++ sn ++ "__.hs") $ show core
    generate  ("test/" ++ file ++ "/" ++ sn ++ ".hs"  ) core
    writeFile ("test/" ++ file ++ "/" ++ sn ++ ".html") $ coreHtml core


transs = ensureInvariants [ConsecutiveFuncs, NoCorePos, NoRecursiveLet, NoCaseDefaultOne]
       . transformExpr removeSeq


-- cannot be done by Overlay since case _ converts to nothing when compiled
removeSeq (CoreApp (CoreFun s) [x,y])
    | s == "Prelude;seq" = CoreCase x [(PatDefault,y)]
removeSeq x = x


liftMain = applyFuncCore f
    where
        f (CoreFunc "main" [] x) = CoreFunc "main" ["real"] (CoreApp x [CoreVar "real"])
        f x = x

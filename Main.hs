
module Main where

import Yhc.Core
import Yhc.Core.Play2
import Generate
import Firstify
import qualified Firstify2.Firstify as F2
import Church
import LambdaLift
import Report
import Unique
import System.Directory
import System.Environment


main = do
    [file] <- getArgs
    createDirectoryIfMissing True file
    core <- loadCore (file ++ ".yca")
    over <- loadCore "library/Overlay.ycr"
    core <- return $ uniqueFuncs $ removeSeq $ traverseCore remCorePos $ coreReachable ["main"] $ coreOverlay core over
    output file 1 core

    putStrLn "Firstifying basic"
    core <- return $ F2.firstify core
    output file 2 core
    putStrLn $ unlines $ report core

    core <- return $ F2.firstifyDataPrepare core
    output file 3 core

    putStrLn "Firstifying scary"
    core <- return $ F2.firstifyData core
    output file 4 core
    putStrLn $ unlines $ report core


output file n core = do
    let sn = show n
    writeFile (file ++ "/" ++ sn ++ "__.hs") $ show core
    generate  (file ++ "/" ++ sn ++ ".hs"  ) core
    writeFile (file ++ "/" ++ sn ++ ".html") $ coreHtml core


removeSeq x = traverseCore f x
    where
        f (CoreApp (CoreFun s) [x,y]) | s == "Prelude.seq" = CoreCase x [(CoreVar "_",y)]
        f x = x

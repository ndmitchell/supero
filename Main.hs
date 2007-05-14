
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


main = do
    createDirectoryIfMissing True "generated"
    core <- loadCore "Example.yca"
    over <- loadCore "library/Overlay.ycr"
    core <- return $ uniqueFuncs $ removeSeq $ traverseCore remCorePos $ coreReachable ["main"] $ coreOverlay core over
    output 1 core

    putStrLn "Firstifying basic"
    core <- return $ F2.firstify core
    output 2 core
    putStrLn $ unlines $ report core


    core <- return $ F2.firstifyDataPrepare core
    output 3 core

    putStrLn "Firstifying scary"
    core <- return $ F2.firstifyData core
    output 4 core
    putStrLn $ unlines $ report core


output n core = do
    let sn = show n
    writeFile ("generated/" ++ sn ++ "__.hs") $ show core
    generate  ("generated/" ++ sn ++ ".hs"  ) core


removeSeq x = traverseCore f x
    where
        f (CoreApp (CoreFun s) [x,y]) | s == "Prelude.seq" = CoreCase x [(CoreVar "_",y)]
        f x = x

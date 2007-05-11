
module Main where

import Yhc.Core
import Yhc.Core.Play2
import Generate
import Firstify
import qualified Firstify2.Firstify as F2
import Church
import LambdaLift
import Report
import System.Directory


main = do
    createDirectoryIfMissing True "generated"
    core <- loadCore "Example.yca"
    over <- loadCore "library/Overlay.ycr"
    core <- return $ traverseCore remCorePos $ coreReachable ["main"] $ coreOverlay core over
    output 1 core

    core <- return $ F2.firstify core
    output 2 core
    putStrLn $ unlines $ report core

    --core <- return $ coreLambdaLift $ church core
    --output 3 core

    --core <- return $ firstify core
    --output 4 core
    

output n core = do
    let sn = show n
    writeFile ("generated/" ++ sn ++ ".txt") $ show core
    generate  ("generated/" ++ sn ++ ".hs" ) core


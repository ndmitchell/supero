
module Main where

import Yhc.Core
import Generate
import Firstify
import Church
import LambdaLift


main = do
    core <- loadCore "Example.yca"
    over <- loadCore "library/Overlay.ycr"
    core <- return $ coreReachable ["main"] $ coreOverlay core over
    output 1 core

    core <- return $ firstify core
    output 2 core

    core <- return $ coreLambdaLift $ church core
    output 3 core

    core <- return $ firstify core
    output 4 core
    

output n core = do
    let sn = show n
    writeFile (sn ++ "generated.txt") $ show core
    generate (sn ++ "generated.hs") core


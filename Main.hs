
module Main where

import Yhc.Core
import Generate


main = do
    core <- loadCore "Example.yca"
    over <- loadCore "library/Overlay.ycr"
    core <- return $ coreReachable ["main"] $ coreOverlay core over
    saveCore "_generated.yca" core
    results core


results core = do
    writeFile "_generated.txt" $ show core
    generate "_generated.hs" core

quickmain = do
    core <- loadCore "_generated.yca"
    results core

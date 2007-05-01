
module Main where

import Yhc.Core
import Generate


main = do
    core <- loadCore "Example.yca"
    over <- loadCore "library/Overlay.ycr"
    core <- return $ coreReachable ["main"] $ coreOverlay core over
    writeFile "_generated.txt" $ show core
    generate "_generated.hs" core

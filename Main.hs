
module Main where

import Yhc.Core
import Generate
import Firstify
import Church


main = do
    core <- loadCore "Example.yca"
    over <- loadCore "library/Overlay.ycr"
    core <- return $ coreReachable ["main"] $ coreOverlay core over
    writeFile "_generated.txt" $ show core
    generate "_generated.hs" core

    core <- return $ firstify core
    writeFile "__generated.txt" $ show core
    generate "__generated.hs" core

    core <- return $ church core
    writeFile "___generated.txt" $ show core
    generate "___generated.hs" core

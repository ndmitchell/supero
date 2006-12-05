
import Yhc.Core
import Convert
import Type
import Normalise
import System.Environment


main = do
    (x:_) <- getArgs
    pm <- loadCore "Primitive.ycr"
    cr <- loadCore x
    let core = coreReachable ["main"] $ coreOverlay cr pm
    print $ pipe $ normalise $ convert core

    where
        -- inline . populate
        pipe = inline . populate -- id -- . populate


import Yhc.Core
import Convert
import Type
import Normalise
import System.Environment


main = do
    (x:_) <- getArgs
    cr <- loadCore x
    print $ pipe $ normalise $ convert cr

    where
        -- inline . populate
        pipe = inline . populate -- id -- . populate

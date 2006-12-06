
import Yhc.Core
import Convert
import Type
import Normalise
import Revert
import System.Environment


main = do
    (x:_) <- getArgs
    pm <- loadCore "Primitive.ycr"
    cr <- loadCore x
    let core = coreReachable ["main"] $ coreOverlay cr pm
        prog = optimise $ convert core
    print $ coreReachable ["main"] $ revert core prog


optimise :: Prog -> Prog
optimise = pipe . normalise
    where pipe = inline . populate

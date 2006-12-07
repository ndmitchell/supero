
import Yhc.Core
import Convert
import Type
import Normalise
import Revert
import System.Environment


main = do
    (x:xs) <- getArgs
    let n = if null xs then 1 else read (head xs)
    pm <- loadCore "Primitive.ycr"
    cr <- loadCore x
    let core = coreReachable ["main"] $ coreOverlay cr pm
        prog = optimise n $ convert core
    print $ coreReachable ["main"] $ revert core prog


optimise :: Int -> Prog -> Prog
optimise n = f n . simplify
    where
        f 0 = id
        f n = pipe . f (n-1)
    
        pipe = inline . populate

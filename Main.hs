
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
    let core = coreReach $ coreOverlay cr pm
        prog = optimise n $ convert core
        core2 = coreReach $ coreInlin $ coreReach $ revert core prog
    print core2


coreReach = coreReachable ["main"]
coreInlin = coreInline Basic


optimise :: Int -> Prog -> Prog
optimise n prog = f n $ simplify prog
    where
        analysis = call_eval_analysis prog
    
        f 0 = id
        f n = pipe . f (n-1)
    
        pipe = case_call . call_eval analysis


import Yhc.Core
import Convert
import Revert
import System.Environment


main = do
    [x] <- getArgs
    pm <- loadCore "Primitive.ycr"
    cr <- loadCore x

    let core = removeRecursiveLet $ applyBodyCore uniqueFreeVars $ coreReach $ coreOverlay cr pm
        prog = convert core
        core2 = coreReach $ coreInlin $ coreReach $ revert core prog
    print $ applyBodyCore uniqueFreeVars $ coreReach $ coreOverlay cr pm


coreReach = coreReachable ["main"]
coreInlin = coreInline InlineAlias

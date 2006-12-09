
import Convert
import Revert
import Type
import System.Environment


main = do
    [x] <- getArgs
    pm <- loadCore "Primitive.ycr"
    cr <- loadCore x

    let core = mapUnderCore remCorePos $ removeRecursiveLet $ uniqueFreeVarsCore $ drop1module $ coreReach $ coreOverlay cr pm
        prog = convert core
        core2 = coreReach $ coreInlin $ coreReach $ revert core prog
    print core2


coreReach = coreReachable ["main"]
coreInlin = coreInline InlineAlias

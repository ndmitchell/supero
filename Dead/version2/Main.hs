
import Convert
import Revert
import Type
import System.Environment
import Data.List
import Data.Char


main = do
    [file] <- getArgs
    pm <- loadCore "Primitive.ycr"
    cr <- loadCore file

    let core = prepare $ coreReach $ coreOverlay cr pm
        prog = convert core
        core2 = coreReach $ revert core prog
        core3 = coreReach $ coreInlin core3
        coreNice = coreReach $ coreInline InlineForward $ core2
        hask = coreToHaskell $ fixPrims core2
    
    print coreNice
    putStr $ unlines $ higherOrderReport coreNice
    
    prefix <- readFile "Prefix.txt"
    writeFile (file ++ ".hs") (prefix ++ hask)
    putStrLn "-- Haskell written out"
    


coreReach = coreReachable ["main"]
coreInlin = coreInline InlineFull


prepare = primCheck . mapUnderCore remCorePos . removeRecursiveLet . uniqueFreeVarsCore . drop1module



primCheck core = core2{coreFuncs = filter (not . isPrimitive . coreFuncBody) (coreFuncs core2)}
    where
        core2 = mapUnderCore f core
    
        f (CoreFun x) | isPrimitive $ coreFuncBody $ coreFunc core x = (CorePrim x)
        f x = x


fixPrims :: Core -> Core
fixPrims = mapUnderCore f
    where
        f (CorePrim xs) = CorePrim $ "prim_" ++ map (\x -> if x == '.' then '_' else x) xs
        f (CoreStr x) = CoreApp (CorePrim "prim_STRING") [CoreStr x]
        f (CoreChr x) = CoreInt (ord x)
        f x = x


higherOrderReport :: Core -> [String]
higherOrderReport core = if null res then ["-- HO: NONE"] else res
    where
        res = concatMap f (coreFuncs core)
        
        f fun = ["-- HO: " ++ x ++ " called with " ++ show nargs ++ ", expecting " ++ show nfun ++ " in " ++ coreFuncName fun
                    | CoreApp (CoreFun x) args <- allCore fun,
                      let func = coreFunc core x,
                      let nargs = length args, let nfun = length (coreFuncArgs func),
                      nargs /= nfun]

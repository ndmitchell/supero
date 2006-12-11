
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
        core2 = coreReach $ coreInlin $ coreReach $ revert core prog
        hask = coreToHaskell $ fixPrims core2
    
    print core2
    
    prefix <- readFile "Prefix.txt"
    writeFile (file ++ ".hs") (prefix ++ coreToHaskell (fixPrims core2))
    putStrLn "-- Haskell written out"
    


coreReach = coreReachable ["main"]
coreInlin = coreInline InlineAlias


prepare = primCheck . mapUnderCore remCorePos . letReduction . removeRecursiveLet . uniqueFreeVarsCore . drop1module



primCheck core = core2{coreFuncs = filter (not . isPrimitive . coreFuncBody) (coreFuncs core2)}
    where
        core2 = mapUnderCore f core
    
        f (CoreFun x) | isPrimitive $ coreFuncBody $ coreFunc core x = (CorePrim x)
        f x = x



letReduction :: Core -> Core
letReduction = mapUnderCore f
    where
        f (CoreLet bind x) = coreLet many (replaceFreeVars once x)
            where (once,many) = partition (\(lhs,rhs) -> countUses lhs x <= 1) bind
        
        f x = x
        

-- Count the number of uses of a free variable        
countUses :: String -> CoreExpr -> Int
countUses s (CoreVar x) = if x == s then 1 else 0

countUses s (CoreCase on alts) = countUses s on + maximum (map g alts)
    where
        g (lhs,rhs) | s `elem` allCoreVar lhs = 0
                    | otherwise = countUses s rhs

countUses s (CoreLet bind x) | s `elem` map fst bind = 0
                             | otherwise = sum $ map (countUses s) (x : map snd bind)

countUses s x = sum $ map (countUses s) (getChildrenCore x)



fixPrims :: Core -> Core
fixPrims = mapUnderCore f
    where
        f (CorePrim xs) = CorePrim $ "prim_" ++ map (\x -> if x == '.' then '_' else x) xs
        f (CoreStr x) = CoreApp (CorePrim "prim_STRING") [CoreStr x]
        f (CoreChr x) = CoreInt (ord x)
        f x = x

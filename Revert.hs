
module Revert(revert) where

import Type
import Data.List
import Data.Maybe


revert :: Core -> CoreEx -> Core
revert core (CoreEx funcs) = core{coreFuncs = main3 : map (revertFunc fs) fs}
    where
        main3 = CoreFunc "main" (coreFuncArgs main) (revertExpr fs main2)
        main2 = CoreApp (CoreFun "main") (map CoreVar (coreFuncArgs main))
        main = coreFunc core "main"
        
        fs = zip [0..] funcs


revertFunc :: [(Int,CoreFuncEx)] -> (Int,CoreFuncEx) -> CoreFunc
revertFunc mapping (n,func) =
    CoreFunc
        (coreFuncExName func ++ "_" ++ show n)
        (argList func)
        (revertExpr mapping $ coreFuncExBody func)


argList :: CoreFuncEx -> [String]
argList func = nub [i | CoreVar i <- concatMap allCore (coreFuncExArgs func)]


-- for each expression, pick the best call sequence
revertExpr :: [(Int,CoreFuncEx)] -> CoreExpr -> CoreExpr
revertExpr mapping x = f x
    where
        f orig@(CoreApp (CoreFun name) args)
                | null funs = error $ "Failed to find a match: " ++ show orig
                | otherwise = res
            where
                res = CoreApp (CoreFun $ name ++ "_" ++ show n) (map (f . fromJust . (`lookup` m)) (argList fun))
                (n,fun,m) = head funs
            
                funs = [(n,fun,m) | (n,fun) <- mapping, coreFuncExName fun == name
                                  , Just m <- [match (coreFuncExArgs fun) args]]
        
        f x = setChildrenCore x $ map f $ getChildrenCore x



-- try doing a unification
match :: [CoreExpr] -> [CoreExpr] -> Maybe [(String,CoreExpr)]
match (x:xs) (y:ys) = do
        r1 <- f x y
        r2 <- match xs ys
        return (r1++r2)
    where
        f (CoreVar x) y = Just [(x,y)]
        f (CoreApp x xs) (CoreApp y ys) = match (x:xs) (y:ys)
        f x y = if x == y then Just [] else Nothing

match [] [] = Just []
match _ _ = Nothing

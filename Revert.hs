
module Revert(revert) where

import Type
import Safe
import Data.List
import Data.Maybe

type Binding = [(String,CoreExpr)]


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
argList = argListArgs . coreFuncExArgs

argListArgs :: [CoreExpr] -> [String]
argListArgs args = nub [i | CoreVar i <- concatMap allCore args]


-- for each expression, pick the best call sequence
revertExpr :: [(Int,CoreFuncEx)] -> CoreExpr -> CoreExpr
revertExpr mapping x = f x
    where
        f orig@(CoreApp (CoreFun name) args) = CoreApp (CoreFun name2) (map f args2)
            where CoreApp (CoreFun name2) args2 = matchCall mapping orig
        
        f (CoreFun name) = f (CoreApp (CoreFun name) [])
        
        f x = setChildrenCore x $ map f $ getChildrenCore x




matchCall :: [(Int,CoreFuncEx)] -> CoreExpr -> CoreExpr
matchCall mapping orig@(CoreApp (CoreFun name) args)
        = if null res then error $ "Failed to find a match: " ++ show orig else best
    where
        lown = minimum (map fst res)
        best = head [b | (a,b) <- res, a == lown]
        
        res = [(n,call) | (n,fun) <- mapping, coreFuncExName fun == name
                        , Just (p,params) <- [matchArgs (coreFuncExArgs fun) args]
                        , let call = CoreApp (CoreFun $ name ++ "_" ++ show n) params]


-- lhs is the left of a function
-- call is the caller
-- return the score as the first item
-- lower is better
matchArgs :: [CoreExpr] -> [CoreExpr] -> Maybe (Int,[CoreExpr])
matchArgs define caller
        | ncaller > ndefine || isNothing bind = Nothing
        | otherwise = Just (nextra, map (`lookupJust` fromJust bind) args)
    where
        (ndefine, ncaller) = (length define, length caller)
        nextra = ndefine - ncaller
        args = reverse $ drop nextra $ reverse $ argListArgs define

        fresh = take nextra $ ['v':show i | i <- [1..]] \\ concatMap collectAllVars (define ++ caller)
        bind = match define (caller ++ map CoreVar fresh)


validMatch :: Binding -> Bool
validMatch = unique . map fst . nub


-- try doing a unification
match :: [CoreExpr] -> [CoreExpr] -> Maybe Binding
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



unique x = length x == length (nub x)

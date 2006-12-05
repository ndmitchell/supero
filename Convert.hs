
module Convert(convert) where

import Type
import Safe
import Yhc.Core
import Data.List
import qualified Data.Map as Map


convert :: Core -> Supero
convert core = Supero (Map.fromList [(funcName x, x) | x <- fs])
    where
        (n,fs) = mapAccumL convertFunc 0 $ coreFuncs $ drop1mod core


drop1mod :: Core -> Core
drop1mod (Core name imports datas funcs) = Core "" [] (map g datas) (map h funcs)
    where
        f x = case break (== '.') x of
                   (_,"") -> x
                   (_,_:xs) -> xs
    
        g (CoreData name free args) = CoreData (f name) free (map g2 args)
        g2 (CoreCtor name items) = CoreCtor (f name) items
        
        h (CoreFunc name args body) = CoreFunc (f name) args (mapOverCore h2 body)
        h2 (CoreFun x) = CoreFun $ f x
        h2 (CoreCon x) = CoreCon $ f x
        h2 x = x




convertFunc :: Int -> CoreFunc -> (Int, Func)
convertFunc n x = (n2, Func (coreFuncName x) [(map Var args2, convertExpr expr2)])
    where
        (n2,args2,expr2) = freshFree (coreFuncArgs x) (coreFuncBody x) n
    

convertExpr :: CoreExpr -> Expr
convertExpr x = case x of
        CorePos _ x -> f x
        CoreCase x xs -> Case (f x) [(f a, f b) | (a,b) <- xs]
        CoreVar x -> Var $ read x
        CoreApp x xs -> Apply (f x) (fs xs)
        CoreCon x -> Ctr x
        CoreFun x -> Fun x
        _ -> error $ show x
    where
        f = convertExpr
        fs = map f
        


-- number the variables as appropriate
freshFree :: [String] -> CoreExpr -> Int -> (Int, [Int], CoreExpr)
freshFree args x n = (n+nvars, map (`lookupJust` rens) args, mapOverCore f x)
    where
        nvars = length vars
        vars = nub $ args ++ [i | CoreVar i <- allCore x]
        
        rens = zip vars [n..]

        f (CoreVar x) = CoreVar $ show $ lookupJust x rens
        f x = x

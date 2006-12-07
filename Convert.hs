
module Convert(convert, drop1mod, insertEval) where

import Type
import Safe
import Yhc.Core
import Data.List
import Data.Play
import qualified Data.Map as Map
import qualified Data.Set as Set


convert :: Core -> Prog
convert core = Prog (Map.fromList [(funcName x, x) | x <- fs])
    where
        (n,fs) = mapAccumL convertFunc 0 $ coreFuncs $ fixPrims $ drop1mod core



fixPrims :: Core -> Core
fixPrims core = core{coreFuncs = mapUnderCore usePrim norm}
    where
        (prim,norm) = partition (isPrim . coreFuncBody) (coreFuncs core)
        prims = Set.fromList (map coreFuncName prim)

        usePrim (CoreFun x) | x `Set.member` prims = CorePrim x
        usePrim x = x
        
        isPrim (CorePos _ x) = isPrim x
        isPrim (CoreApp x []) = isPrim x
        isPrim (CoreVar x) = x == "primitive"
        isPrim _ = False


drop1mod :: Core -> Core
drop1mod (Core name imports datas funcs) = Core name imports (map g datas) (concatMap h funcs)
    where
        f x = case break (== '.') x of
                   (_,"") -> x
                   (_,_:xs) -> xs
    
        g (CoreData name free args) = CoreData (f name) free (map g2 args)
        g2 (CoreCtor name items) = CoreCtor (f name) items
        
        h (CoreFunc name args body) 
            | name == "main" = []
            | otherwise = [CoreFunc (f name) args (mapOverCore h2 body)]
        h2 (CoreFun x) = CoreFun $ f x
        h2 (CoreCon x) = CoreCon $ f x
        h2 x = x




convertFunc :: Int -> CoreFunc -> (Int, Func)
convertFunc n x = (n2, Func (coreFuncName x) [FuncAlt 0 (map Var args2) (insertEval $ convertExpr expr2)])
    where
        (n2,args2,expr2) = freshFree (coreFuncArgs x) (coreFuncBody x) n
    

convertExpr :: CoreExpr -> Expr
convertExpr x = case x of
        CorePos _ x -> f x
        CoreCase x xs -> Case (f x) [(f a, f b) | (a,b) <- xs]
        CoreVar x -> Var $ read x
        CoreApp x xs -> Apply (f x) (fs xs)
        CoreCon x -> Ctr x
        CoreFun x -> Fun x 0
        CorePrim x -> Prim x
        
        CoreStr x -> Const $ ConstStr x
        CoreInt x -> Const $ ConstInt x
        CoreInteger x -> Const $ ConstInteger x
        
        _ -> error $ "Convert.convertExpr: " ++ show x
    where
        f = convertExpr
        fs = map f


insertEval :: Expr -> Expr
insertEval = mapUnder f
    where
        f (Apply (Fun x n) xs) = Apply (Fun x n) (map g xs)
            where
                vars = [i | Var i <- xs]
                badvars = nub $ vars \\ nub vars
                
                g (Var i) = if i `elem` badvars then Eval (Var i) else Var i
                g x = Eval x

        f x = x


-- number the variables as appropriate
freshFree :: [String] -> CoreExpr -> Int -> (Int, [Int], CoreExpr)
freshFree args x n = (n+nvars, map (`lookupJust` rens) args, mapOverCore f x)
    where
        nvars = length vars
        vars = nub $ args ++ [i | CoreVar i <- allCore x]
        
        rens = zip vars [n..]

        f (CoreVar x) = CoreVar $ show $ lookupJust x rens
        f x = x

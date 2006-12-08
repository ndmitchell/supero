
module Convert(convert, drop1mod) where

import Type
import Safe
import Yhc.Core
import Data.List
import Data.Play
import qualified Data.Map as Map
import qualified Data.Set as Set


convert :: Core -> Prog
convert core = Prog (Map.fromList [(funcName x, x) | x <- concat fs])
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




convertFunc :: Int -> CoreFunc -> (Int, [Func])
convertFunc n x = (n2, map f funcs2)
    where
        (n2,args2,expr2) = freshFree (coreFuncArgs x) (coreFuncBody x) n
        funcs2 = removeLets (CoreFunc (coreFuncName x) (map show args2) expr2)
        
        f (CoreFunc name args body) = Func name [FuncAlt 0 (map (Var . read) args) (convertExpr body)]
    

convertExpr :: CoreExpr -> Expr
convertExpr x = case x of
        CorePos _ x -> f x
        CoreCase x xs -> Case (f x) [(f a, f b) | (a,b) <- xs]
        CoreVar x -> Var $ read x
        CoreApp x xs -> Apply (f x) (fs xs)
        CoreCon x -> Ctr x
        CoreFun x -> Fun x
        CorePrim x -> Prim x
        
        CoreStr x -> Const $ ConstStr x
        CoreInt x -> Const $ ConstInt x
        CoreInteger x -> Const $ ConstInteger x
        
        _ -> error $ "Convert.convertExpr: " ++ show x
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
        f (CoreLet binds x) = CoreLet [(show $ lookupJust a rens, b) | (a,b) <- binds] x
        f x = x



-- algorithm:
-- find each let, give it the number x, being its first variable
-- pass all free variables at that point
removeLets :: CoreFunc -> [CoreFunc]
removeLets (CoreFunc name args body2) = res
    where
        body = mapOverCore g body2
            where
                g (CoreLet [x] y) = CoreLet [x] y
                g (CoreLet (x:xs) y) = CoreLet [x] $ g $ CoreLet xs y
                g x = x
    
        res = CoreFunc name args (use body) : map gen lets
        lets = [x | x@(CoreLet{}) <- allCore body]
        
        gen (CoreLet binds body) = CoreFunc (name ++ "#" ++ fst (head binds)) free (use body)
            where free = freeVars body
            
        use x = mapOverCore f x
            where
                f (CoreLet binds body) = CoreApp (CoreFun (name ++ "#" ++ fst (head binds))) (map g free)
                    where
                        free = freeVars body
                        
                        g x = case lookup x binds of
                                  Just y -> y
                                  Nothing -> CoreVar x
                f x = x


freeVars :: CoreExpr -> [String]
freeVars x = nub $ f x
    where
        f (CoreLet bind x) = (f x ++ concatMap (f . snd) bind) \\ map fst bind
        f (CoreCase on alts) = f on ++ concatMap g alts
        f (CoreVar x) = [x]
        f x = concatMap f $ getChildrenCore x
        
        g (lhs,rhs) = f rhs \\ f lhs

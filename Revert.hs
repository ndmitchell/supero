
module Revert(revert) where

import Yhc.Core
import Match
import Type
import Data.Play
import Data.Maybe
import Data.List
import Convert
import qualified Data.Map as Map


revert :: Core -> Prog -> Core
revert core (Prog funcs) = core2{coreFuncs = concatMap (revertFunc funcs) (Map.elems funcs)}
    where core2 = drop1mod core


revertFunc :: FuncMap -> Func -> [CoreFunc]
revertFunc funcs func = concatMap (revertAlt funcs (funcName func)) (funcAlts func)


revertAlt :: FuncMap -> String -> FuncAlt -> [CoreFunc]
revertAlt funcs name (FuncAlt n lhs rhs) = newfunc : newmain
    where
        newname = name ++ "_" ++ show n
        newfunc = CoreFunc newname args (f rhs)
        newmain = [CoreFunc "main" args (CoreApp (CoreFun newname) (map CoreVar args)) | name == "main" && n == 0]
    
        args = map toVar $ listArgs lhs
        toVar i = "v" ++ show i
        
        
        f :: Expr -> CoreExpr
        f (Const (ConstStr x)) = CoreStr x
        f (Const (ConstInt x)) = CoreInt x
        f (Ctr x) = CoreCon x
        f (Var x) = CoreVar $ toVar x
        f (Prim x) = CorePrim x
        f (Apply (Fun x) xs) = callFun x xs
        f (Fun x) = f (Apply (Fun x) [])
        f (Apply x xs) = CoreApp (f x) (map f xs)
        f (Case on alts) = CoreCase (f on) [(f a, f b) | (a,b) <- alts]
        f x = error $ "Revert.revertAlt.f: " ++ show x
        
        
        callFun name args = CoreApp (CoreFun (name ++ "_" ++ show n)) (map f $ bindArgs ++ extraArgs)
            where
                extraArgs = drop (length lhs) args
                bindArgs = [x | Just x <- map (`lookup` bind) frees]
            
                frees = listArgs lhs
                func = fromJust $ Map.lookup name funcs
                lhs = head [altMatch alt | alt <- funcAlts func, altNum alt == n]

                (n,bind,_) = case findBestRhs func args of
                                 Just x -> x
                                 Nothing -> (0, zip frees args, undefined)



listArgs lhs = nub [n | Var n <- concatMap allOver lhs]

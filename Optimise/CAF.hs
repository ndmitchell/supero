
module Optimise.CAF(detectCafs, decaffeinate) where

import Yhc.Core
import qualified Data.Set as Set


detectCafs :: Core -> Set.Set CoreFuncName
detectCafs core = Set.fromList [coreFuncName x | x <- coreFuncs core, isCaf (coreFuncMap fm) x]
    where fm = toCoreFuncMap core


isCaf func (CoreFunc name [] body) = expensive $ coreSimplify body
    where
        expensive (CoreCon x) = False
        expensive (CoreFun x) = False
        expensive (CoreLit x) = False
        expensive (CoreApp (CoreCon x) xs) = any expensive xs
        expensive (CoreApp (CoreFun x) xs) = not $ unsaturated func x xs
        expensive x = error $ show ("missed",x)

isCaf _ _ = False


unsaturated :: (CoreFuncName -> CoreFunc) -> CoreFuncName -> [CoreExpr] -> Bool
unsaturated func name args = f [] name (length args)
    where
        f seen name args | name `elem` seen = False
                         | args == 0 || arity > args = True
                         | isCoreFunc x = g (name:seen) (coreFuncBody x) (args - arity)
                         | otherwise = False
            where
                x = func name
                arity = coreFuncArity x

        g seen (CoreApp (CoreFun name) args) extra = f seen name (length args + extra)
        g seen (CoreFun name) extra = f seen name extra
        g seen (CorePos _ x) extra = g seen x extra
        g _ _ _ = False


decaffeinate :: Set.Set CoreFuncName -> Core -> Core
decaffeinate cafs core =
        core{coreFuncs = newPrims ++ map f (coreFuncs core)}
    where
        argCAF = "realWorld#"
        newPrims = [prim "skipCAF" 2, prim "realWorld#" 0]
        prim name arity = CorePrim name arity [] [] False []

        fake = Set.fromList [name | CoreFunc name [] _ <- coreFuncs core
                                  , name `Set.notMember` cafs]

        f (CoreFunc name args body) = CoreFunc name 
            (if faker then ["uncaf"] else args)
            (if faker then CoreApp (CoreFun "skipCAF") [CoreVar "uncaf",bod] else bod)
            where
                faker = name `Set.member` fake
                bod = transform g body
        f x = x

        g (CoreFun x) | x `Set.member` fake = CoreApp (CoreFun x) [CoreFun argCAF]
        g x = x

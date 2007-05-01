
module Simplify(simplify) where

import Yhc.Core
import Data.List
import Data.Maybe
import Type(disjoint)


simplify :: Core -> CoreExpr -> CoreExpr
simplify core = mapUnderCore f
    where
        f (CoreCase (CoreFun x) alts) = f (CoreCase (CoreApp (CoreFun x) []) alts)
        
        f orig@(CoreApp (CoreCase _ _) _) = f $ CoreCase on (map g alts)
            where
                CoreApp (CoreCase on alts) args = uniqueExpr orig
                g (lhs,rhs) = (lhs, f $ CoreApp rhs args)
        
        f (CoreCase (CoreCase on alts1) alts2) = f $ CoreCase on (map g alts1)
            where
                g (lhs,rhs) = (h lhs, f $ CoreCase (h rhs) alts2)
                    where
                        h x = replaceFreeVars (zip vs (map CoreVar vars)) x
                        vs = allCoreVar lhs
                        vars = freeVars 'v' \\ (collectAllVars lhs ++ collectAllVars rhs)
        
        f (CoreCase (CoreLet bind on) alts) = f $ CoreLet bind (f $ CoreCase on alts)
        
        f (CoreLet bind x) = coreLet many (mapUnderCore f $ replaceFreeVars once x)
            where
                (once,many) = partition (\(lhs,rhs) -> isSimple rhs || countVar lhs x <= 1) bind
                
                isSimple (CoreApp x []) = isSimple x
                isSimple (CoreFun x) = True
                isSimple (CorePos x y) = isSimple y
                isSimple (CoreVar x) = True
                isSimple (CoreApp (CorePos _ (CoreFun name)) args) = isSimple (CoreApp (CoreFun name) args)
                isSimple (CoreApp (CoreFun name) args) = all isSimple args && length args < nfunc
                    where nfunc = length $ coreFuncArgs $ coreFunc core name
                isSimple _ = False
                
        
        f (CoreLet binds (CoreCase on alts1))
            | disjoint [i | CoreVar i <- allCore on] (map fst binds) = f $ CoreCase on (map g alts1)
            where g (lhs,rhs) = (lhs,f $ coreLet (filter ((`notElem` allCoreVar lhs) . fst) binds) $ f rhs)
        
        f (CoreCase (CoreCon con) alts) = f $ CoreCase (CoreApp (CoreCon con) []) alts
        
        f (CoreCase on@(CoreApp (CoreCon con) fields) alts)
                | not $ null matches = head matches
            where
                matches = mapMaybe g alts
        
                g (CoreCon x, rhs) | x == con = Just rhs
                g (CoreApp (CoreCon x) xs, rhs) | x == con = Just $ replaceFreeVars (zip (map fromCoreVar xs) fields) rhs
                g (CoreVar x,rhs) = Just $ replaceFreeVars [(x,on)] rhs
                g _ = Nothing

        f (CoreApp (CoreApp x xs) ys) = f $ CoreApp x (xs++ys)

        f x = x


uniqueExpr :: CoreExpr -> CoreExpr
uniqueExpr x = uniqueFreeVarsWithout (collectAllVars x) x


freeVars :: Char -> [String]        
freeVars c = [c:show i | i <- [1..]]


module Firstify2.Firstify where

import Yhc.Core
import Yhc.Core.Play2
import Unique
import Firstify2.SpecExpr
import Firstify2.SpecState
import Control.Monad.State
import Data.List


firstify :: Core -> Core
firstify = coreFix . specMain False specExpr


firstifyData :: Core -> Core
firstifyData = coreFix . specMain True specExpr


coreFix :: Core -> Core
coreFix = coreReachable ["main"] . coreInline InlineCallOnce



firstifyDataPrepare :: Core -> Core
firstifyDataPrepare core2 = core{coreFuncs = newfuncs ++ oldfuncs}
    where
        core = traverseCore simp $ coreSimplify core2
        (oldfuncs,(uid,newfuncs)) = runState (mapM f $ coreFuncs core) (uniqueFuncsMin core,[])

        f (CoreFunc name args body) = liftM (CoreFunc name args) $ traverseCoreM (g name) body
        f x = return x
        
        g name (CoreCase on alts) = liftM (CoreCase on) $ mapM (h name) alts
        g name x = return x
        
        h name (CoreApp (CoreCon c) args, rhs) | not $ null args = do
            (uid,fs) <- get
            let newname = uniqueName name uid
                vars = map fromCoreVar args
                free = collectFreeVars rhs \\ vars
                newargs = free ++ vars
                expr = (CoreApp (CoreCon c) args, CoreApp (CoreFun newname) (map CoreVar newargs))
                func = CoreFunc newname newargs rhs
            put (uid+1, func:fs)
            return expr
        h name x = return x


simp x@(CoreCase (CoreVar on) alts) | on `elem` collectFreeVars (CoreCase (CoreInt 0) alts) =
        CoreCase (CoreVar on) (map f alts)
    where
        f (lhs,rhs) = (lhs, replaceFreeVars [(on,lhs)] rhs)

simp (CoreLet [] x) = x
simp (CoreLet ((a,b):bs) x) = reduceBind a b $ simp $ CoreLet bs x

simp x = x



reduceBind lhs rhs (CoreCase on alts) | lhs `notElem` collectFreeVars on = simp $ CoreCase on $ map f alts
    where
        f (alhs,arhs) | lhs `notElem` collectFreeVars alhs = (alhs, reduceBind lhs rhs arhs)
        f x = x

reduceBind lhs rhs x = CoreLet [(lhs,rhs)] x


{-
simp (CoreLet ((a,b):bs) x) = reduceLet a b $ simp $ CoreLet bs x

| not $ null yes = coreLet no $ traverseCore simp $ replaceFreeVars yes x
    where
        (yes,no) = partition ((<= 1) . flip countFreeVar x . fst) binds
-}
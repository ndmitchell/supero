
module Firstify2.Firstify where

import Yhc.Core
import Yhc.Core.Play2
import Unique
import Firstify2.SpecExpr
import Firstify2.SpecState
import Control.Monad.State
import Data.List


firstify :: Core -> Core
firstify = coreReachable ["main"] . specMain False specExpr


firstifyData :: Core -> Core
firstifyData = coreReachable ["main"] . specMain True specExpr



firstifyDataPrepare :: Core -> Core
firstifyDataPrepare core = core{coreFuncs = newfuncs ++ oldfuncs}
    where
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

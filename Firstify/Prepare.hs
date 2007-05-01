
module Firstify.Prepare(prepare) where

import Yhc.Core hiding (collectAllVars,collectFreeVars,uniqueBoundVars,replaceFreeVars)
import Yhc.Core.FreeVar2

import Data.List
import Data.Maybe
import qualified Data.Map as Map


prepare :: Core -> CoreFuncMap
prepare = lambdas . zeroApp . toCoreFuncMap . removeRecursiveLet . mapUnderCore remCorePos


-- insert explicit lambdas
lambdas :: CoreFuncMap -> CoreFuncMap
lambdas fm = Map.map (applyBodyFunc $ mapUnderCore f) fm
    where
        f orig@(CoreApp (CoreFun name) args) | extra > 0 =
                CoreLam new (CoreApp (CoreFun name) (args ++ map CoreVar new))
            where
                extra = arity fm name - length args
                new = take extra $ freeVars 'v' \\ collectAllVars orig
        f x = x


-- make sure all applications are explicit
zeroApp :: CoreFuncMap -> CoreFuncMap
zeroApp = Map.map $ applyBodyFunc $ mapUnderCore f
    where
        f (CoreFun  x) = CoreApp (CoreFun  x) []
        f (CorePrim x) = CoreApp (CorePrim x) []
        f (CoreApp (CoreApp x ys) zs) = CoreApp x (ys++zs)
        f x = x


arity :: CoreFuncMap -> CoreFuncName -> Int
arity fm = length . coreFuncArgs . fromMaybe (error "arity") . coreFuncMapMaybe fm


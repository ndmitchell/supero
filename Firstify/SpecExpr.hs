
module Firstify.SpecExpr(specExpr) where

import Yhc.Core hiding (collectAllVars, collectFreeVars, replaceFreeVars, countFreeVar)
import Yhc.Core.FreeVar3
import Firstify.SpecState
import Firstify.Template

import Data.Maybe


specExpr :: CoreExpr -> Spec CoreExpr
specExpr = coreSimplifyExprUniqueExt spec


spec :: (CoreExpr -> Spec CoreExpr) -> CoreExpr -> Spec CoreExpr
spec cont (CoreApp (CoreFun err) xs) | err == "Prelude.error"
    = return $ CoreApp (CoreFun err) (take 1 xs)

spec cont (CoreFun x) = spec cont (CoreApp (CoreFun x) [])
spec cont o@(CoreApp (CoreFun x) xs) = do
    CoreApp (CoreFun name) args <- applyTemplate o
    sat <- isSaturated name args
    inline <- shouldInline name
    if sat && inline then do
        (_,func) <- getFunc name
        y <- duplicateExpr $ fromJust $ coreInlineFunc func args
        transformM cont y
     else
        duplicateExpr $ coreApp (CoreFun name) args

spec cont x@(CoreCase on alts) | isCoreFun $ fst $ fromCoreApp on = do
    res <- applyTemplate x
    duplicateExpr res

spec cont x = return x

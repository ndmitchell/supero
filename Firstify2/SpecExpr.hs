
module Firstify2.SpecExpr(specExpr) where

import Yhc.Core
import Yhc.Core.Play2
import Firstify2.Spec

import Control.Monad
import Data.Maybe
import Data.List


specExpr :: CoreExpr -> Spec CoreExpr
specExpr = traverseCoreM spec


isDull x = isCoreVar x || isCoreCon x || isCoreCase x || isCorePos x


spec :: CoreExpr -> Spec CoreExpr
spec x | isDull x || isCoreConst x = return x
spec (CoreApp x xs) | isDull x = return $ CoreApp x xs

spec (CoreApp (CoreFun err) xs) | err == "Prelude.error"
    = return $ CoreApp (CoreFun err) (take 1 xs)

spec o@(CoreFun x) = spec (CoreApp o [])

spec o@(CoreApp (CoreFun x) xs) = do
        i <- getArity x
        (t,u) <- mapAndUnzipM templateArg xs
        if all isTempNone t && length xs <= i
            then checkInline x xs
            else do
                name <- getTemplate (Template x t)
                specFunc name
                checkInline name (concat u)
    where
        checkInline name args = do
            sat <- isSaturated name args
            inline <- shouldInline name
            if sat && inline then do
                func <- getFunc name
                specExpr $ fromJust $ coreInlineFunc func args
             else
                return $ coreApp (CoreFun name) args


spec (CoreApp (CoreApp x xs) ys) = spec $ CoreApp x (xs++ys)

spec (CoreLet bind x) = do
    res <- mapM shouldInlineLet bind
    let (inline,keep) = divide res bind
    x2 <- if null inline
          then return x
          else specExpr $ replaceFreeVars inline x
    return $ coreLet keep x2

spec x = error $ show ("spec - todo",x)


-- the choice of what to do on let is very varied. We can:
-- 1) inline all functions
-- 2) inline all functions which occur once
-- 3) inline all functions which occur once per branch
--
-- currently we pick 1
shouldInlineLet :: (CoreVarName, CoreExpr) -> Spec Bool
shouldInlineLet (lhs,rhs) =
    case fromCoreApp rhs of
        (CoreFun x, xs) -> liftM not $ isSaturated x xs
        _ -> return False


-- the template representing it, and how you would invoke the templated version
templateArg :: CoreExpr -> Spec (TempArg, [CoreExpr])
templateArg o@(CoreApp (CoreFun x) xs) = do
    i <- getArity x
    if i <= length xs
        then return (TempNone, [o])
        else return (TempApp x (length xs), xs)

templateArg (CoreFun x) = templateArg (CoreApp (CoreFun x) [])
templateArg x = return (TempNone,[x])



divide :: [Bool] -> [a] -> ([a],[a])
divide res xs = (map snd x, map snd y)
    where (x,y) = partition fst $ zip res xs


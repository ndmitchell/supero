
module Firstify2.SpecExpr(specExpr) where

import Yhc.Core
import Yhc.Core.Play2
import Firstify2.SpecState
import Firstify2.Template

import Control.Monad
import Data.Maybe
import Data.List


specExpr :: CoreExpr -> Spec CoreExpr
specExpr = traverseCoreM spec


isDull x = isCoreVar x || isCoreCon x || isCorePos x


spec :: CoreExpr -> Spec CoreExpr
spec (CoreApp (CoreFun err) xs) | err == "Prelude.error"
    = return $ CoreApp (CoreFun err) (take 1 xs)

spec o@(CoreFun x) = spec (CoreApp o [])

spec o@(CoreApp (CoreFun x) xs) = do
        temp <- createTemplate x xs
        case temp of
            Nothing -> checkInline x xs
            Just y -> do
                addTemplate y
                CoreApp (CoreFun name) args <- useTemplate y xs
                checkInline name args
    where
        checkInline name args = do
            sat <- isSaturated name args
            inline <- shouldInline name
            if sat && inline then do
                func <- getFunc name
                specExpr $ fromJust $ coreInlineFunc func args
             else
                return $ coreApp (CoreFun name) args

spec x@(CoreCase on _) | isCoreCon $ fst $ fromCoreApp on =
    return $ coreSimplifyCaseCon x

spec (CoreApp (CoreApp x xs) ys) = spec $ CoreApp x (xs++ys)

spec (CoreApp (CoreCase on alts) xs) = liftM (CoreCase on) (mapM f alts)
    where f (lhs,rhs) = liftM ((,) lhs) $ spec $ coreApp rhs xs

spec (CoreLet bind x) = do
    res <- mapM shouldInlineLet bind
    let (inline,keep) = divide res bind
    x2 <- if null inline
          then return x
          else specExpr $ replaceFreeVars inline x
    return $ coreLet keep x2

spec x | isDull x || isCoreConst x || isCoreCase x = return x
spec (CoreApp x xs) | isDull x = return $ CoreApp x xs

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



divide :: [Bool] -> [a] -> ([a],[a])
divide res xs = (map snd x, map snd y)
    where (x,y) = partition fst $ zip res xs



module Firstify2.SpecExpr(specExpr) where

import Yhc.Core hiding (collectAllVars, replaceFreeVars)
import Yhc.Core.FreeVar2
import Yhc.Core.Play2
import Firstify2.SpecState
import Firstify2.Template

import Control.Monad
import Data.Maybe
import Data.List
import Debug.Trace


specExpr :: CoreExpr -> Spec CoreExpr
specExpr = traverseCoreM spec


isDull x = isCoreVar x || isCoreCon x || isCorePos x


spec :: CoreExpr -> Spec CoreExpr
spec (CoreApp (CoreFun err) xs) | err == "Prelude.error"
    = return $ CoreApp (CoreFun err) (take 1 xs)

spec o@(CoreFun x) = spec (CoreApp o [])

spec o@(CoreApp (CoreFun x) xs) = do
    CoreApp (CoreFun name) args <- applyTemplate o
    sat <- isSaturated name args
    inline <- shouldInline name
    if sat && inline then do
        (_,func) <- getFunc name
        specExpr $ fromJust $ coreInlineFunc func args
     else
        return $ coreApp (CoreFun name) args

spec x@(CoreCase on _) | isCoreCon $ fst $ fromCoreApp on =
    spec $ coreSimplifyCaseCon x

spec x@(CoreCase (CoreCase _ _) _) = traverseCoreM spec $ coreSimplifyCaseCase x

spec x@(CoreCase on alts) | isCoreFun $ fst $ fromCoreApp on = applyTemplate x

spec (CoreApp (CoreApp x xs) ys) = spec $ CoreApp x (xs++ys)

spec (CoreApp (CoreCase on alts) xs) = liftM (CoreCase on) (mapM f alts)
    where f (lhs,rhs) = liftM ((,) lhs) $ spec $ coreApp rhs xs

-- breaks sharing and may break free variables
spec (CoreApp (CoreLet bind x) ys) = spec . CoreLet bind =<< spec (CoreApp x ys)

spec o@(CoreCase (CoreLet bind on) alts) = traverseCoreM spec $ coreSimplifyCaseLet o

spec (CoreLet [] x) = return x
spec (CoreLet (b1:b2:bs) x) = spec (CoreLet (b2:bs) x) >>= spec . (CoreLet [b1])

spec o@(CoreLet [(lhs,CoreLet bind rhs)] x) = do
    inner <- spec $ CoreLet [(lhs,rhs)] x
    spec $ CoreLet bind inner

spec o@(CoreLet [(lhs,rhs)] x) = do
        let (fn,args) = fromCoreApp rhs
            (newargs,newbinds) = unzip $ runFreeVars $ deleteVars (collectAllVars o) >> mapM promote args
            newrhs = coreApp fn newargs
        b <- isSpecData
        unsat <- if isCoreFun fn then liftM not $ isSaturated (fromCoreFun fn) args
                 else if isCoreCon fn then return b
                 else return False

        -- you can do two things - inline, or strength reduce
        -- strength reduce will not duplicate expressions
        let inline = unsat
            reduce = False

        if inline || reduce
            then specExpr $ coreLet (concat newbinds) $
                    if inline then
                        replaceFreeVars [(lhs,newrhs)] x
                    else
                        error "reduce here"
            else return o
    where
        promote (CoreVar x) = return (CoreVar x, [])
        promote x = do i <- getVar
                       return (CoreVar i, [(i,x)])


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


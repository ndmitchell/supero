
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
        if res == x then return x else spec res
    where
        res = coreSimplifyCaseCon x

spec x@(CoreCase (CoreCase _ _) _) = traverseCoreM spec $ coreSimplifyCaseCase x

spec x@(CoreCase on alts) | isCoreFun $ fst $ fromCoreApp on = applyTemplate x

spec (CoreApp (CoreApp x xs) ys) = spec $ CoreApp x (xs++ys)

spec (CoreApp (CoreCase on alts) xs) = liftM (CoreCase on) (mapM f alts)
    where f (lhs,rhs) = liftM ((,) lhs) $ spec $ coreApp rhs xs

-- breaks sharing and may break free variables
spec (CoreApp (CoreLet bind x) ys) = spec . CoreLet bind =<< spec (CoreApp x ys)

spec o@(CoreCase (CoreLet bind on) alts) = specExpr $ coreSimplifyCaseLet o

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

        -- you can do two things - inline, or strength reduce
        -- strength reduce will not duplicate expressions
        inline <- if isCoreFun fn then liftM not $ isSaturated (fromCoreFun fn) args
                  else if isCoreCon fn then return b
                  else return False

        reduce <- case fn of
                      CoreFun x -> do
                          Arity _ b2 <- getArity (fromCoreFun fn)
                          return $ b && b2
                      _ -> return False

        if inline || reduce
            then specExpr $ coreLet (concat newbinds) $
                    if inline then
                        replaceFreeVars [(lhs,newrhs)] x
                    else
                        reducer lhs newrhs x
            else return o
    where
        promote (CoreVar x) = return (CoreVar x, [])
        promote x = do i <- getVar
                       return (CoreVar i, [(i,x)])


spec x | isDull x || isCoreConst x || isCoreCase x = return x
spec (CoreApp x xs) | isDull x = return $ CoreApp x xs

spec x = error $ show ("spec - todo",x)



divide :: [Bool] -> [a] -> ([a],[a])
divide res xs = (map snd x, map snd y)
    where (x,y) = partition fst $ zip res xs


-- reduce the strength of the given binding
-- but do NOT reduce the sharing
reducer :: CoreVarName -> CoreExpr -> CoreExpr -> CoreExpr
reducer lhs rhs x = error $ show ("reducer",lhs,rhs,x)


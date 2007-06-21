
module Firstify.SpecExpr(specExpr) where

import Yhc.Core hiding (collectAllVars, collectFreeVars, replaceFreeVars, countFreeVar)
import Yhc.Core.FreeVar2
import Firstify.SpecState
import Firstify.Template

import Control.Monad
import Data.Maybe
import Data.List
import Debug.Trace

specExpr :: CoreExpr -> Spec CoreExpr
specExpr x = do
        res <- specs x
        let extra = not $ null $ post \\ pre
            pre = collectFreeVars x
            post = collectFreeVars res
        b <- isSpecData
        () <- if b && not (check res) then trace (show (x,res)) $ return () else return ()
        if extra then error (show (x,res)) else return res


check x = null [() | CoreCase on alts <- universe x, a <- alts, f a]
    where
        f (CoreApp _ xs, CoreApp _ ys) = any (`elem` vars) free
            where
                vars = map fromCoreVar xs
                free = concatMap collectFreeVars $ reverse $ drop (length xs) $ reverse ys
        f _ = False


isDull x = isCoreVar x || isCoreCon x || isCorePos x


specs = transformM spec


spec :: CoreExpr -> Spec CoreExpr
spec (CoreApp (CoreFun err) xs) | err == "Prelude.error"
    = return $ CoreApp (CoreFun err) (take 1 xs)

spec o@(CoreFun x) = spec (CoreApp o [])

spec o@(CoreApp x xs) | any isCoreLet (x:xs) = do
        let (a:as,binds) = unzip $ runFreeVars $ deleteVars (collectAllVars o) >> mapM f (x:xs)
        inner <- spec $ CoreApp a as
        spec $ coreLet (concat binds) inner
    where
        f (CoreLet [(lhs,rhs)] x) = do
            (x2,bs) <- f x
            lhs2 <- getVar
            return (replaceFreeVars [(lhs,CoreVar lhs2)] x2, (lhs2,rhs) : bs)
        f x = return (x, [])

spec o@(CoreApp (CoreFun x) xs) = do
    CoreApp (CoreFun name) args <- applyTemplate o
    sat <- isSaturated name args
    inline <- shouldInline name
    if sat && inline then do
        (_,func) <- getFunc name
        specs $ fromJust $ coreInlineFunc func args
     else
        return $ coreApp (CoreFun name) args

{-
-- dangerous at this point, breaks the invariant
spec x@(CoreCase (CoreVar on) alts) | on `elem` collectFreeVars (CoreCase (CoreInt 0) alts) =
        specs $ CoreCase (CoreVar on) (map f alts)
    where
        f (lhs,rhs) = (lhs, replaceFreeVars [(on,lhs)] rhs)
-}

spec x@(CoreCase on _) | isCoreCon $ fst $ fromCoreApp on =
        if res == x then error "failed to match case" else spec res
    where
        res = coreSimplifyCaseCon x

spec x@(CoreCase (CoreCase _ _) _) = transformM spec $ coreSimplifyCaseCase x

spec x@(CoreCase on alts) | isCoreFun $ fst $ fromCoreApp on = applyTemplate x

spec (CoreApp (CoreApp x xs) ys) = spec $ CoreApp x (xs++ys)

spec (CoreApp (CoreCase on alts) xs) = liftM (CoreCase on) (mapM f alts)
    where f (lhs,rhs) = liftM ((,) lhs) $ spec $ coreApp rhs xs

spec o@(CoreCase (CoreLet bind on) alts) = specs $ coreSimplifyCaseLet o

spec (CoreLet [] x) = return x
spec (CoreLet (b1:b2:bs) x) = spec (CoreLet (b2:bs) x) >>= spec . (CoreLet [b1])

spec o@(CoreLet [(lhs,CoreLet bind rhs)] x) = do
    inner <- spec $ CoreLet [(lhs,rhs)] x
    spec $ CoreLet bind inner

spec o@(CoreLet [(lhs,rhs)] x) 
    | countFreeVar lhs x <= 1 = specs $ replaceFreeVars [(lhs,rhs)] x
    | otherwise = do
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

        if inline then
            specs $ coreLet (concat newbinds) $ replaceFreeVars [(lhs,newrhs)] x
            --specs $ replaceFreeVars [(lhs,rhs)] x
         else if reduce then
            let res = coreLet (concat newbinds) $ reducer lhs newrhs x
            in if res == o then return o else specs res
            --specs $ replaceFreeVars [(lhs,rhs)] x
         else do
            res <- specs $ replaceFreeVars [(lhs,rhs)] x
            -- () <- trace (show (o,res)) $ return ()
            --return o
            specs $ replaceFreeVars [(lhs,rhs)] x
            return o
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
reducer lhs rhs x
    | True || countFreeVar lhs x <= 1 = replaceFreeVars [(lhs,rhs)] x
    | otherwise = reduceStrength $ deleteUnusedLets x
    where
        deleteUnusedLets x = x
        reduceStrength x = CoreLet [(lhs,rhs)] x


module Firstify2.Template(
    applyTemplate,
    createTemplate, addTemplate, useTemplate
    ) where

import Yhc.Core hiding (collectAllVars, collectFreeVars)
import Yhc.Core.FreeVar2
import Firstify2.SpecState
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Debug.Trace
import Firstify2.Terminate



applyTemplate :: CoreExpr -> Spec CoreExpr
applyTemplate x = do
    t <- createTemplate x
    case t of
        Nothing -> return x
        Just y -> do
            func <- getFunc (templateName y)
            if isCoreFunc $ snd func then do
                addTemplate y
                useTemplate y x
             else
                return x


createTemplate :: CoreExpr -> Spec (Maybe Template)
createTemplate (CoreApp (CoreFun name) args) = do
        (ar,fn) <- getFunc name
        let valid targs = length args > coreFuncArity fn || not (all isTempNone targs)

        targs <- mapM f args
        if not (valid targs)
            then return Nothing
            else do
                t <- weakenTemplate $ TemplateApp name targs
                case t of
                    Just t@(TemplateApp _ targs) | valid targs -> return $ Just t
                    _ -> return Nothing
    where
        f (CoreFun x) = f (CoreApp (CoreFun x) [])
        f (CoreApp (CoreFun x) xs) = do
            Arity i dat <- getArity x
            b <- isSpecData
            return $ if i > length xs || (b && dat)
                then TempApp x (length xs)
                else TempNone

        f (CoreApp (CoreCon x) xs) = do
            b <- isSpecData
            xs2 <- mapM f xs
            return $ if not b && all isTempNone xs2 then TempNone else TempCon x xs2

        f x = return TempNone

createTemplate (CoreCase on alts) | isCoreFun name = do
        b <- isSpecData
        (_,func) <- getFunc $ fromCoreFun name
        if b && isCoreFunc func
            then weakenTemplate $ TemplateCase (fromCoreFun name) (length onargs) (map f alts)
            else return Nothing
    where
        (name,onargs) = fromCoreApp on
    
        f (CoreVar _, y) = ("", TempNone)
        f (CoreCon x, y) = (x,  TempNone)
        f (CoreApp (CoreCon x) [], y) = f (CoreCon x, y)
        f (CoreApp (CoreCon x) xs, CoreApp (CoreFun y) ys) | xs `isSuffixOf` ys = (x, TempApp y (length ys - length xs))
        f x = error $ "createTemplate, " ++ show (CoreCase on alts) ++ " gives rise to: " ++ show x

createTemplate _ = return Nothing



useTemplate :: Template -> CoreExpr -> Spec CoreExpr
useTemplate t@(TemplateApp name args) (CoreApp (CoreFun n) xs) | n == name = do
        newname <- return . fromJust . Map.lookup t . template =<< get
        return $ CoreApp (CoreFun newname) (concat $ fs args xs)
    where
        fs [] [] = []
        fs (x:xs) (y:ys) = f x y : fs xs ys

        f TempNone x = [x]
        f (TempApp a b) (CoreApp (CoreFun x) xs) | a == x && length xs == b = xs
        f ab (CoreFun x) = f ab (CoreApp (CoreFun x) [])
        f (TempCon a b) (CoreApp (CoreCon x) xs) | a == x = concat $ fs b xs

useTemplate t@(TemplateCase name args alts) (CoreCase on alt) | app == name = do
        newname <- return . fromJust . Map.lookup t . template =<< get
        return $ CoreApp (CoreFun newname) (concat (zipWith f alts alt) ++ args)
    where
        (CoreFun app,args) = fromCoreApp on

        f (_,TempNone) (_,x) = [x]
        f (_,TempApp _ n) (_,CoreApp (CoreFun _) xs) = take n xs


genTemplate :: CoreFuncName -> Template -> Spec CoreFunc
genTemplate newname (TemplateApp oldname tempargs) = do
        (_, func@(CoreFunc _ oldargs oldbody)) <- getFunc oldname
        let args = runFreeVars $ deleteVars (oldargs ++ collectAllVars oldbody) >> mapM f tempargs
            vars = concatMap collectAllVars args
            (extra,newbody) = coreInlineFuncLambda func args
        return $ CoreFunc newname (vars ++ extra) newbody
    where
        f :: TempArg -> FreeVar CoreExpr
        f TempNone = getVar >>= return . CoreVar
        f (TempCon c xs) = mapM f xs >>= return . CoreApp (CoreCon c)
        f (TempApp a i) = replicateM i (liftM CoreVar getVar) >>= return . CoreApp (CoreFun a)

genTemplate newname (TemplateCase oldname extra alts) = do
        (_, func@(CoreFunc _ oldargs oldbody)) <- getFunc oldname
        cr <- liftM core get
        let (CoreCase on2@(CoreApp _ vars2) alts2) = runFreeVars $ deleteVars (oldargs ++ collectAllVars oldbody) >> f cr
            vars = collectFreeVars (CoreCase (CoreFun "") alts2) ++ collectFreeVars on2
            bod2 = CoreCase (fromJust $ coreInlineFunc func vars2) alts2
        return $ CoreFunc newname vars bod2
    where
        f :: Core -> FreeVar CoreExpr
        f cr = do
            a <- replicateM extra getVar
            b <- mapM (g cr) alts
            return $ CoreCase (CoreApp (CoreFun oldname) (map CoreVar a)) b

        g :: Core -> (CoreCtorName,TempArg) -> FreeVar (CoreExpr,CoreExpr)
        g cr (c,TempNone) = do
            i <- getVar
            return (if null c then CoreVar "_" else CoreCon c, CoreVar i)
        g cr (c,TempApp fn i) = do
            j <- replicateM i getVar
            k <- replicateM (length $ coreCtorFields $ coreCtor cr c) getVar
            return (CoreApp (CoreCon c) (map CoreVar k), CoreApp (CoreFun fn) (map CoreVar $ j ++ k))


addTemplate :: Template -> Spec ()
addTemplate t = do
    s <- get
    when (not $ Map.member t (template s)) $ do
        let newname = uniqueJoin (templateName t) (uid s)
        newfunc <- genTemplate newname t
        put $ s{uid = uid s + 1
               ,info = Map.insert newname (Arity 0 False,newfunc) (info s)
               ,template = Map.insert t newname (template s)
               }

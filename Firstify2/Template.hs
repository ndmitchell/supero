
module Firstify2.Template(
    applyTemplate,
    createTemplate, addTemplate, useTemplate
    ) where

import Yhc.Core hiding (collectAllVars)
import Unique
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
            addTemplate y
            useTemplate y x


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
            xs2 <- mapM f xs
            return $ if all isTempNone xs2 then TempNone else TempCon x xs2

        f x = return TempNone

createTemplate (CoreCase on alts) | isCoreFun name = do
        b <- isSpecData
        if b then weakenTemplate $ TemplateCase (fromCoreFun name) (length onargs) (map f alts)
             else return Nothing
    where
        (name,onargs) = fromCoreApp on
    
        f (CoreVar _, y) = ("", TempNone)
        f (CoreCon x, y) = (x,  TempNone)
        f (CoreApp (CoreCon x) xs, CoreApp (CoreFun y) ys) | xs `isSuffixOf` ys = (x, TempApp y (length ys - length xs))

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



addTemplate :: Template -> Spec ()
addTemplate t@(TemplateApp name args) = do
    s <- get
    when (not $ Map.member t (template s)) $ do
        let newname = uniqueName name (uid s)
        newfunc <- genTemplate newname t
        put $ s{uid = uid s + 1
               ,info = Map.insert newname (Arity 0 False,newfunc) (info s)
               ,template = Map.insert t newname (template s)
               }


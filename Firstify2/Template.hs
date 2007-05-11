
module Firstify2.Template(
    createTemplate, addTemplate, useTemplate
    ) where

import Yhc.Core hiding (collectAllVars)
import Unique
import Yhc.Core.FreeVar2
import Firstify2.SpecState
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace



createTemplate :: CoreFuncName -> [CoreExpr] -> Spec (Maybe Template)
createTemplate name args = do
        i <- getArity name
        t <- mapM f args
        return $ if all isTempNone t && length args <= i
                 then Nothing
                 else Just $ Template name t
    where
        f (CoreFun x) = f (CoreApp (CoreFun x) [])
        f (CoreApp (CoreFun x) xs) = do
            i <- getArity x
            return $ if i <= length xs
                then TempNone
                else TempApp x (length xs)

        f (CoreApp (CoreCon x) xs) = do
            xs2 <- mapM f xs
            return $ if all isTempNone xs2 then TempNone else TempCon x xs2

        f x = return TempNone



useTemplate :: Template -> [CoreExpr] -> Spec CoreExpr
useTemplate t@(Template name args) xs = do
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
genTemplate newname (Template oldname tempargs) = do
        CoreFunc _ oldargs oldbody <- getFunc oldname
        let noldargs = length oldargs
            ntempargs = length tempargs
            freevars = runFreeVars $ deleteVars oldargs >> deleteVars (collectAllVars oldbody) >> getVars
        return $ case noldargs `compare` ntempargs of
            EQ -> f freevars oldargs oldbody tempargs
            GT -> f freevars oldargs oldbody (take noldargs $ tempargs ++ repeat TempNone)
            LT -> f left (oldargs ++ used) (coreApp oldbody (map CoreVar used)) tempargs
                where (used,left) = splitAt (ntempargs - noldargs) freevars
    where
        f free oldargs oldbody tempargs = CoreFunc newname newargs newbody
            where
                lst = zip oldargs $ allocateVars free tempargs

                newargs = concatMap arg lst
                arg (_,(TempApp{},x)) = x
                arg (x,(TempNone,_)) = [x]

                newbody = coreLet (concatMap bind lst) oldbody
                bind (v,(TempApp name _,vars)) = [(v,coreApp (CoreFun name) (map CoreVar vars))]
                bind (_,(TempNone,_)) = []

genTemplate x y = error $ "Cannot generate template for primitive: " ++ show x ++ ", with " ++ show y

allocateVars :: [CoreVarName] -> [TempArg] -> [(TempArg, [CoreVarName])]
allocateVars vars tmp = runFreeVars $ putVars vars >> mapM (\x -> liftM ((,) x) (f x)) tmp
    where
        f TempNone = return []
        f (TempApp _ i) = replicateM i getVar




addTemplate :: Template -> Spec ()
addTemplate t@(Template name args) = do
    s <- get
    when (not $ Map.member t (template s)) $ do
        let newname = uniqueName name (uid s)
        newfunc <- genTemplate newname t
        put $ s{uid = uid s + 1
               ,info = Map.insert newname newfunc (info s)
               ,template = Map.insert t newname (template s)
               }


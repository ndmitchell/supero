
module Firstify2.Spec(
    getArity, getTemplate, isSaturated, shouldInline,
    module Firstify2.SpecState
    ) where

import Unique
import Yhc.Core hiding (collectAllVars)
import Yhc.Core.FreeVar2
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

import Debug.Trace

import Firstify2.SpecState



-- it is possible that getArity will ONLY ever be called after specFunc
-- in which case one or isDone or isPending MUST be true
-- not sure if this is correct though
getArity :: CoreFuncName -> Spec Int
getArity = retrieve (return . coreFuncArity)


getTemplate :: Template -> Spec CoreFuncName
getTemplate t@(Template name args) = do
    s <- get
    case Map.lookup t (template s) of
        Just y -> return y
        Nothing -> do
            let newfunc = genTemplate (uid s) (fromJust $ Map.lookup name (info s)) args
                newname = coreFuncName newfunc
            put $ s{uid = uid s + 1
                   ,info = Map.insert newname newfunc (info s)
                   ,template = Map.insert t newname (template s)
                   }
            return newname

-- generate a modified CoreFunc with a new name
genTemplate :: Int -> CoreFunc -> [Maybe TemplateArg] -> CoreFunc
genTemplate uid (CoreFunc oldname oldargs oldbody) tempargs =
        let noldargs = length oldargs
            ntempargs = length tempargs
        in case noldargs `compare` ntempargs of
            EQ -> f freevars oldargs oldbody tempargs
            GT -> f freevars oldargs oldbody (take noldargs $ tempargs ++ repeat Nothing)
            LT -> f left (oldargs ++ used) (coreApp oldbody (map CoreVar used)) tempargs
                where (used,left) = splitAt (ntempargs - noldargs) freevars
    where
        freevars = runFreeVars $ deleteVars oldargs >> deleteVars (collectAllVars oldbody) >> getVars
        newname = uniqueName oldname uid

        f free oldargs oldbody tempargs = CoreFunc newname newargs newbody
            where
                lst = zip oldargs $ allocateVars free tempargs

                newargs = concatMap arg lst
                arg (x,(Nothing,_)) = [x]
                arg (_,(Just _ ,x)) = x
            
                newbody = coreLet (concatMap bind lst) oldbody
                bind (_,(Nothing,_)) = []
                bind (v,(Just (TemplateArg name _),vars)) = [(v,coreApp (CoreFun name) (map CoreVar vars))]

genTemplate _ x y = error $ "Cannot generate template for primitive: " ++ show x ++ ", with " ++ show y


allocateVars :: [CoreVarName] -> [Maybe TemplateArg] -> [(Maybe TemplateArg, [CoreVarName])]
allocateVars vars tmp = runFreeVars $ putVars vars >> mapM f tmp
    where
        f Nothing = return (Nothing,[])
        f x@(Just (TemplateArg _ i)) = liftM ((,) x) (replicateM i getVar)



isSaturated :: CoreFuncName -> [CoreExpr] -> Spec Bool
isSaturated name args = do
    i <- getArity name
    return $ length args >= i


shouldInline :: CoreFuncName -> Spec Bool
shouldInline name = do
        func <- getFunc name
        res <- if isCoreFunc func then isCtor $ coreFuncBody func
                                  else return False
        return res
    where
        isCtor x@(CoreApp (CoreCon _) _) = isCtorApp x
        isCtor _ = return False

        isCtorApp (CoreApp (CoreCon x) xs) = liftM or $ mapM isCtorApp xs
        isCtorApp (CoreApp (CoreFun x) xs) = liftM not $ isSaturated x xs
        isCtorApp (CoreFun x) = isCtorApp (CoreApp (CoreFun x) [])
        isCtorApp _ = return False


module Firstify2.Spec where

import Unique
import Yhc.Core hiding (collectAllVars)
import Yhc.Core.FreeVar2
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set


type Spec a = State SpecState a

data SpecState = SpecState
    {arities :: [(CoreFuncName,Int)] -- those arities given when in Pending
    ,info :: CoreFuncMap -- information about functions
    ,pending :: Set.Set CoreFuncName -- those on the stack
    ,done :: Set.Set CoreFuncName -- those which have been done
    ,template :: Map.Map Template CoreFuncName -- templates created
    ,uid :: Int
    ,localSpecExpr :: CoreExpr -> Spec CoreExpr
    }


-- the function being called, along with the arguments being passed
-- Nothing means the argument is simple (first-order)
data Template = Template CoreFuncName [Maybe TemplateArg]
                deriving (Show,Eq,Ord)

-- an argument, and the number of extra variables it is given
data TemplateArg = TemplateArg CoreFuncName Int
                   deriving (Show,Eq,Ord)

-- map (+1) xs = Template "map" [Just (TemplateArg "+" 1), Nothing]
-- map id xs = Template "map" [Just (TemplateArg "id" 0), Nothing]

isPending :: CoreFuncName -> Spec Bool
isPending name = get >>= return . Set.member name . pending

isDone :: CoreFuncName -> Spec Bool
isDone name = get >>= return . Set.member name . done

addPending :: CoreFuncName -> Spec ()
addPending name = modify $ \s -> s{pending = Set.insert name (pending s)}

delPending :: CoreFuncName -> Spec ()
delPending name = modify $ \s -> s{pending = Set.delete name (pending s)}

addDone :: CoreFuncName -> Spec ()
addDone name = modify $ \s -> s{done = Set.insert name (done s)}


getArity :: CoreFuncName -> Spec Int
getArity name =
    do
        don <- isDone name
        pen <- isPending name
        if don then
            answer
         else if pen then do
            i <- answer
            modify $ \s -> s{arities = (name,i) : arities s}
            return i
         else do
            specFunc name
            answer
    where
        answer = return . coreFuncArity . fromJust . Map.lookup name . info =<< get


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


allocateVars :: [CoreVarName] -> [Maybe TemplateArg] -> [(Maybe TemplateArg, [CoreVarName])]
allocateVars vars tmp = runFreeVars $ putVars vars >> mapM f tmp
    where
        f Nothing = return (Nothing,[])
        f x@(Just (TemplateArg _ i)) = liftM ((,) x) (replicateM i getVar)


specFunc :: CoreFuncName -> Spec ()
specFunc name = do
    addPending name
    s <- get
    let func = fromJust $ Map.lookup name (info s)
        f = localSpecExpr s
    bod <- f $ coreFuncBody func
    modify $ \s -> s{info = Map.insert name (func{coreFuncBody=bod}) (info s)}
    delPending name
    addDone name


specMain :: (CoreExpr -> Spec CoreExpr) -> CoreFuncMap -> CoreFuncMap
specMain coreExpr fm = info $ execState (specFunc "main") s0
    where
        s0 = SpecState [] fm Set.empty Set.empty Map.empty 0 coreExpr

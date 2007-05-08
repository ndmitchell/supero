
module Firstify2.Spec where

import Unique
import Yhc.Core
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
    ,template :: Map.Map (CoreFuncName,Template) CoreFuncName -- templates created
    ,uid :: Int
    ,localSpecExpr :: CoreExpr -> Spec CoreExpr
    }


data Template = Template [Maybe (Int,CoreFuncName)]
                deriving (Show,Eq,Ord)



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
        answer = return . coreArity . fromJust . Map.lookup name . info =<< get


coreArity x = length (coreFuncArgs x) + f (coreFuncBody x)
    where
        f (CoreLam x y) = length x + f y
        f _ = 0


getTemplate :: CoreFuncName -> Template -> Spec CoreFuncName
getTemplate name temp = do
    s <- get
    case Map.lookup (name,temp) (template s) of
        Just y -> return y
        Nothing -> do
            let newfunc = genTemplate (uid s) (fromJust $ Map.lookup name (info s)) temp
                newname = coreFuncName newfunc
            put $ s{uid = uid s + 1
                   ,info = Map.insert newname newfunc (info s)
                   ,template = Map.insert (name,temp) newname (template s)
                   }
            return newname

-- generate a modified CoreFunc with a new name
genTemplate :: Int -> CoreFunc -> Template -> CoreFunc
genTemplate uid func temp = CoreFunc newname todo todo
    where
        todo = error "genTemplate: todo"
        newname = uniqueName (coreFuncName func) uid


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

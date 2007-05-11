
module Firstify2.SpecState where

import Yhc.Core
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace


type Spec a = State SpecState a

data SpecState = SpecState
    {guess :: [Guess] -- information given when in Pending
    ,info :: CoreFuncMap -- information about functions
    ,pending :: Set.Set CoreFuncName -- those on the stack
    ,done :: Set.Set CoreFuncName -- those which have been done
    ,template :: Map.Map Template CoreFuncName -- templates created
    ,uid :: Int
    
    -- methods in here to break cycles
    ,localSpecExpr :: CoreExpr -> Spec CoreExpr
    }

-- given the function, and what you expected, do you get it
data Guess = Guess CoreFuncName String (CoreFunc -> Spec Bool)


-- the function being called, along with the arguments being passed
-- Nothing means the argument is simple (first-order)
data Template = Template CoreFuncName [TempArg]
                deriving (Show,Eq,Ord)

-- an argument, and the number of extra variables it is given
data TempArg = TempApp CoreFuncName Int
             | TempNone
               deriving (Show,Eq,Ord)

isTempNone = (==) TempNone

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


retrieve :: (Eq i, Show i) => (CoreFunc -> Spec i) -> CoreFuncName -> Spec i
retrieve generate name = do
    pending <- isPending name
    if pending then do
        res <- answer
        let newguess = Guess name (show res) (liftM (== res) . generate)
        modify $ \s -> s{guess = newguess : guess s}
        return res
     else do
        done <- isDone name
        s <- get
        when (not done) $ specFunc name
        answer
    where
        answer = getFunc name >>= generate


specFunc :: CoreFuncName -> Spec ()
specFunc name = do
    -- () <- trace name $ return ()
    b1 <- isDone name
    b2 <- isPending name
    when (not b1 && not b2) $ do
        addPending name
        func <- getFunc name
        when (isCoreFunc func) $ do
            s <- get
            bod <- localSpecExpr s $ coreFuncBody func
            modify $ \s -> s{info = Map.insert name (func{coreFuncBody=bod}) (info s)}
        delPending name
        addDone name


specMain :: (CoreExpr -> Spec CoreExpr) -> CoreFuncMap -> CoreFuncMap
specMain coreExpr fm = info $ execState (specFunc "main") s0
    where
        s0 = SpecState [] fm Set.empty Set.empty Map.empty 0 coreExpr


getFunc :: CoreFuncName -> Spec CoreFunc
getFunc name = return . fromJust . Map.lookup name . info =<< get

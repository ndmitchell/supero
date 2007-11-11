
module Optimise.State where

import qualified Data.Map as Map
import Control.Monad.State
import Yhc.Core.UniqueId
import Yhc.Core


---------------------------------------------------------------------
-- MONAD


type StateIO state result = StateT state IO result


sioRun :: StateIO state result -> state -> IO (result,state)
sioRun x state = runStateT x state

instance (Monad m, UniqueId i) => UniqueIdM (StateT i m) where
    getIdM = liftM getId get
    putIdM n = modify (putId n)

sioPrint :: String -> StateIO state ()
sioPrint s = liftIO $ putStrLn s


sioPause :: StateIO state ()
sioPause = do
    c <- liftIO getChar
    when (c /= '\n') $ error "done"


---------------------------------------------------------------------
-- STATE


data S = S {names :: Map.Map CoreExpr CoreFuncName
           ,funcs :: [CoreFunc]
           ,nameId :: Int
           ,uniqueId :: Int
           ,core :: CoreFuncName -> CoreFunc
           ,prim :: CoreFuncName -> Bool
           ,caf :: CoreFuncName -> Bool -- an expensive caf
           ,term :: Termination
           }

instance UniqueId S where
    getId = uniqueId
    putId i x = x{uniqueId = i}


type SS a = StateIO S a


---------------------------------------------------------------------
-- TERMINATION


data Context = Context
        {current :: CoreExpr    -- the current expression under test
        ,current_ :: [CoreExpr] -- the list of expressions since last residuation
        ,rhoCall :: [CoreExpr]  -- those which got expanded (were a CoreFun)
        ,rhoResid :: [CoreExpr] -- those which got residuated
        ,rho_ :: [CoreExpr]     -- a list of all expressions ever
        }

emptyContext = Context (CoreVar "") [] [] [] []

type Termination = S -> Context -> Maybe CoreExpr


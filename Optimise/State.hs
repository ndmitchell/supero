
module Optimise.State where

import qualified Data.Map as Map
import Control.Monad.State
import System.IO
import Yhc.Core.UniqueId
import Yhc.Core
import Data.Homeomorphic.SimpleParallel as H
import Optimise.Util


---------------------------------------------------------------------
-- MONAD


type StateIO state result = StateT state IO result


sioRun :: StateIO state result -> state -> IO (result,state)
sioRun x state = runStateT x state

instance (Monad m, UniqueId i) => UniqueIdM (StateT i m) where
    getIdM = liftM getId get
    putIdM n = modify (putId n)

sioPutStrLn :: String -> StateIO state ()
sioPutStrLn = liftIO . putStrLn

sioPutStr :: String -> StateIO state ()
sioPutStr = liftIO . putStr

sioLog :: String -> StateIO S ()
sioLog x = do
    s <- get
    liftIO $ hPutStrLn (logging s) x

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
           ,logging :: Handle
           }

instance UniqueId S where
    getId = uniqueId
    putId i x = x{uniqueId = i}


type SS a = StateIO S a


---------------------------------------------------------------------
-- TERMINATION


data Context = Context
        -- the current expression under test
        {current :: CoreExpr
        
        -- the expressions since the last residuation
        -- includes the result just after residuation
        -- does not include the current expression
        -- ,currents :: [CoreExpr]
        
        -- includes all expressions pre their unfoldings
        -- a strict superset of currents
        -- does not include current
        ,rho  :: Homeomorphic CoreExpr1 CoreExpr     -- a list of all expressions ever
        }

emptyContext = Context undefined H.empty

addContext :: Context -> CoreExpr -> Context
addContext context x = context{rho=H.insert (coreExprShellBlur x) x (rho context)}

-- clear the currents field
resetContext = id

type Termination = Context -> SS (Maybe CoreExpr)


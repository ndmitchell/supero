
module StateFail where

import Control.Monad.State
import Control.Monad.Error
import Yhc.Core.UniqueId


type StateFail state failure result = StateT state (ErrorT String IO) result



sfRun :: (Show failure, Read failure) => StateFail state failure result -> state -> IO (Either failure (result,state))
sfRun x state = do
    res <- runErrorT (runStateT x state)
    return $ case res of
         Left failure -> Left (read failure)
         Right (result,state) -> Right (result,state)

sfFail :: Show failure => failure -> StateFail state failure result
sfFail failure = lift $ fail (show failure)


instance (Monad m, UniqueId i) => UniqueIdM (StateT i m) where
    getIdM = liftM getId get
    putIdM n = modify (putId n)

sfPrint :: String -> StateFail state failure ()
sfPrint s = liftIO $ putStrLn s



module StateFail where

import Control.Monad.State
import Control.Monad.Error
import Yhc.Core.UniqueId


type StateFail state failure result = StateT state (Either String) result


sfRun :: (Show failure, Read failure) => StateFail state failure result -> state -> Either failure (result,state)
sfRun x state = case runStateT x state of
                    Left failure -> Left (read failure)
                    Right (result,state) -> Right (result,state)


sfFail :: Show failure => failure -> StateFail state failure result
sfFail failure = lift $ fail (show failure)


instance (Monad m, UniqueId i) => UniqueIdM (StateT i m) where
    getIdM = liftM getId get
    putIdM n = modify (putId n)


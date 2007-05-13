
module Firstify2.Terminate(weakenTemplate) where

import Firstify2.SpecState
import Debug.Trace


weakenTemplate :: Template -> Spec (Maybe Template)
weakenTemplate (Template n [x,y]) | n == name = return $ Just $ Template name [x,TempNone]
    where name = "Prelude.Prelude.Prelude.1107.showPosInt"

weakenTemplate x = return $ Just x

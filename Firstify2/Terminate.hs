
module Firstify2.Terminate(weakenTemplate) where

import Firstify2.SpecState
import Data.List
import Debug.Trace


weakenTemplate :: Template -> Spec (Maybe Template)
weakenTemplate (Template n (x1:y:xs)) | name `isPrefixOf` n = return $ Just $ Template n (x1:TempNone:xs)
    where name = "Prelude.Prelude.Prelude.1107.showPosInt"

weakenTemplate x = return $ Just x

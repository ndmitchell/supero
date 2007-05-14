
module Firstify2.Terminate(weakenTemplate) where

import Firstify2.SpecState
import Data.List
import Debug.Trace


weakenTemplate :: Template -> Spec (Maybe Template)
weakenTemplate (TemplateApp n (x1:y:xs)) | name `isPrefixOf` n = return $ Just $ TemplateApp n (x1:TempNone:xs)
    where name = "Prelude.Prelude.Prelude.1107.showPosInt"

weakenTemplate x = return $ Just x

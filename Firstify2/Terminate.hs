
module Firstify2.Terminate(weakenTemplate) where

import Firstify2.SpecState
import Data.List
import Debug.Trace


weakenTemplate :: Template -> Spec (Maybe Template)
weakenTemplate (TemplateApp n xs@(_:_)) | name `isPrefixOf` n = return $ Just $ TemplateApp n xs2
    where
        name = "Prelude.Prelude.Prelude.1107.showPosInt"
        xs2 = init xs ++ [TempNone]

weakenTemplate x = return $ Just x

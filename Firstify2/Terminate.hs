
module Firstify2.Terminate(weakenTemplate) where

import Firstify2.SpecState


weakenTemplate :: Template -> Spec (Maybe Template)
weakenTemplate x = return $ Just x

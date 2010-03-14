
module Terminate(terminate, (<|), (<=|), newHistory, History, (+=)) where

import Type
import Debug.Trace


data History = History [Exp]

newHistory = History []


terminate :: (Exp -> Exp -> Bool) -> History -> Exp -> Bool
terminate _ (History _) _ = False

(<|) :: Exp -> Exp -> Bool
(<|) = error "<|"



(<=|) :: Exp -> Exp -> Bool
(<=|) = error "<=|"



(+=) :: Exp -> History -> History
(+=) x (History xs) = trace (pretty x) $ History $ x:xs

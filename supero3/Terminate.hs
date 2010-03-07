
module Terminate(terminate, (<|), (<=|), newHistory, History, (+=)) where

import Type


newtype History = History [Exp]

newHistory = History []


terminate :: (Exp -> Exp -> Bool) -> History -> Exp -> Bool
terminate _ _ _ = False

(<|) :: Exp -> Exp -> Bool
(<|) = error "<|"



(<=|) :: Exp -> Exp -> Bool
(<=|) = error "<=|"



(+=) :: Exp -> History -> History
(+=) x (History xs) = History $ x:xs

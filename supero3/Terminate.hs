
module Terminate(terminate, (<|), (<=|), History, (+:)) where


newtype History = History [Exp]



terminate :: (Exp -> Exp -> Bool) -> History -> Exp -> Bool


(<|) :: Exp -> Exp -> Bool



(<=|) :: Exp -> Exp -> Bool



(+:) :: Exp -> History -> History
(+:) x (History xs) = History $ x:xs

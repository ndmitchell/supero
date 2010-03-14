
module Terminate(terminate, (<|), (<=|), newHistory, History, (+=)) where

import Type
import Debug.Trace
import Data.List


data History = History [Exp]

newHistory = History []


terminate :: (Exp -> Exp -> Bool) -> History -> Exp -> Bool
terminate (<) (History hist) x = if not $ all (x <) hist then True {- error "here" -} else False
--    where
--        info = error $ prettyNames (head hist) ++ "\n AGAINST \n" ++ prettyNames x ++ "\n" ++ show (getBag x ,getBag y)
--        y = head hist


(<|) :: Exp -> Exp -> Bool
x <| y = nub x1 /= nub y1 || length x1 < length y1
    where x1 = getBag x
          y1 = getBag y
(<=|) :: Exp -> Exp -> Bool
x <=| y = x <| y || getBag x == getBag y


(+=) :: Exp -> History -> History
(+=) x (History xs) = trace (prettyNames x) $ History $ x:xs



getBag :: Exp -> [Name]
getBag x = sort $ map (getName . snd) bind
    where FlatExp _ bind _ = toFlat x

bagEquality x y = x == y

bagSubset x y = null (x \\ y) && not (null $ y \\ x)

setSupset x y = bagSubset (nub y) (nub x)

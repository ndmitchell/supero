
module Terminate(terminate, (<|), (<=|), newHistory, History, (+=), progress) where

import Type
import Debug.Trace
import Data.List


data History = History Int [Exp] [Bag]
type Bag = [Name]

newHistory = History 0 [] []


progress :: History -> String -> Bool
progress (History n _ _) msg = trace (msg ++ " = " ++ show n) False


terminate :: (Bag -> Bag -> Bool) -> History -> Exp -> Bool
terminate (<) (History _ hs bs) x = if not $ all (getBag x <) bs then trace "terminate" True else False
    where
        bad = head $ filter (not . (getBag x <) . getBag) hs
        info = error $ prettyNames bad ++
               "\n WHEN TRYING TO ADD\n" ++ prettyNames x ++
               "\n BECAUSE OF\n" ++ show (getBag x \\ getBag bad) ++ "\n" ++
               show ("<",getBag x < getBag bad,"==",x==bad,"bageq",getBag x == getBag bad,"<|",getBag x <| getBag bad,"<=|",getBag x <=| getBag bad)
    
--    where
--        info = error $ prettyNames (head hist) ++ "\n AGAINST \n" ++ prettyNames x ++ "\n" ++ show (getBag x ,getBag y)
--        y = head hist


(<|), (<=|) :: Bag -> Bag -> Bool
x <| y = nub x /= nub y || length x < length y
x <=| y = x <| y || x == y


(+=) :: Exp -> History -> History
(+=) x (History n xs bs) = {- trace (prettyNames x) $ -} History (n+1) (x:xs) (getBag x : bs)



getBag :: Exp -> [Name]
getBag x = sort $ map (getName . snd) bind
    where FlatExp _ bind _ = toFlat x

bagEquality x y = x == y

bagSubset x y = null (x \\ y) && not (null $ y \\ x)

setSupset x y = bagSubset (nub y) (nub x)

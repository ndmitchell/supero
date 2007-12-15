
module Data.Homeomorphic.Internal where



data Shell a = Shell a Int [Shell a]
               deriving (Eq, Ord)

shell :: a -> [Shell a] -> Shell a
shell a b = Shell a (length b) b

shellRoot :: Shell a -> (a,Int)
shellRoot (Shell a b c) = (a,b)

shellEq :: Eq a => Shell a -> Shell a -> Bool
shellEq a b = shellRoot a == shellRoot b



(<<|) :: Eq a => Shell a -> Shell a -> Bool
(<<|) x y = dive x y || couple x y

dive :: Eq a => Shell a -> Shell a -> Bool
dive x (Shell _ _ ys) = any (x <<|) ys

couple :: Eq a => Shell a -> Shell a -> Bool
couple (Shell x1 x2 x3) (Shell y1 y2 y3) = x1 == y1 && x2 == y2 && and (zipWith (<<|) x3 y3)

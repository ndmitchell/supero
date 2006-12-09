
module General where

import Data.List


unique :: Eq a => [a] -> Bool
unique x = length x == length (nub x)


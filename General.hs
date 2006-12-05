
module General where

import Data.List


unique :: Eq a => [a] -> Bool
unique x = length x == length (nub x)


fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

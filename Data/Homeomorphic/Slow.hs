
module Data.Homeomorphic.Slow where

import Data.Homeomorphic.Internal

data Homeomorphic k v = Homeomorphic [(Shell k, v)]


insert :: Ord k => Shell k -> v -> Homeomorphic k v -> Homeomorphic k v
insert k v (Homeomorphic xs) = Homeomorphic ((k,v):xs)

find :: Ord k => Shell k -> Homeomorphic k v -> [v]
find k (Homeomorphic xs) = [b | (a,b) <- xs, a <<| k]

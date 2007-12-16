
module Data.Homeomorphic.Simple where

import Data.Homeomorphic.Internal
import Data.Maybe

data Homeomorphic k v = Homeomorphic [(Shell k, v)]


empty :: Homeomorphic k v
empty = Homeomorphic []

insert :: Ord k => Shell k -> v -> Homeomorphic k v -> Homeomorphic k v
insert k v (Homeomorphic xs) = Homeomorphic ((k,v):xs)

find :: Ord k => Shell k -> Homeomorphic k v -> [v]
find k (Homeomorphic xs) = [b | (a,b) <- xs, a <<| k]

findOne :: Ord k => Shell k -> Homeomorphic k v -> Maybe v
findOne k = listToMaybe . find k

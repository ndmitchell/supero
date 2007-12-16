
module Data.Homeomorphic.Hash where

import qualified Data.Set as Set
import Data.Homeomorphic.Internal
import Data.Maybe
import Data.List(foldl')

data Homeomorphic k v = Homeomorphic [(Shell k, Hash k, v)]


empty :: Homeomorphic k v
empty = Homeomorphic []

insert :: Ord k => Shell k -> v -> Homeomorphic k v -> Homeomorphic k v
insert k v (Homeomorphic xs) = Homeomorphic ((k,calcHash k,v):xs)

find :: Ord k => Shell k -> Homeomorphic k v -> [v]
find k (Homeomorphic xs) = [c | (a,b,c) <- xs, check b, a <<| k]
    where check = checkHash k

findOne :: Ord k => Shell k -> Homeomorphic k v -> Maybe v
findOne k = listToMaybe . find k



---------------------------------------------------------------------

type Hash k = Set.Set (Int,k)

calcHash :: Ord k => Shell k -> Hash k
calcHash = f Set.empty
    where f x (Shell a b c) = foldl' f (Set.insert (b,a) x) c

-- important: calculate the hash of y only once per invokation
checkHash :: Ord k => Shell k -> Hash k -> Bool
checkHash y = (`Set.isSubsetOf` calcHash y)

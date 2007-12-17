
module Data.Homeomorphic.Hash1 where

import qualified Data.Set as Set
import Data.Homeomorphic.InternalHash
import Data.List(foldl')

type Homeomorphic k v = HomeomorphicWith k v (Hash k)
empty = emptyWith
insert x = insertWith calcHash x
find x = findWith checkHash x
findOne x = findOneWith checkHash x

---------------------------------------------------------------------

type Hash k = Set.Set (Int,k)

calcHash :: Ord k => Shell k -> Hash k
calcHash = f Set.empty
    where f x (Shell a b c) = foldl' f (Set.insert (b,a) x) c

-- important: calculate the hash of y only once per invokation
checkHash :: Ord k => Shell k -> Hash k -> Bool
checkHash y = (`Set.isSubsetOf` calcHash y)


module Data.Homeomorphic.Hash2 where

import qualified Data.IntMap as IntMap
import Data.Homeomorphic.InternalHash
import Data.List(foldl')

type Homeomorphic k v = HomeomorphicWith k v (Hash k)
empty = emptyWith
insert x = insertWith calcHash x
find x = findWith checkHash x
findOne x = findOneWith checkHash x

---------------------------------------------------------------------

-- how many of each arity
type Hash k = [(Int,Int)]

calcHash :: Ord k => Shell k -> Hash k
calcHash = IntMap.toAscList . f IntMap.empty
    where f x (Shell a b c) = foldl' f (IntMap.insertWith (\_ i -> i+1) b 1 x) c

-- important: calculate the hash of y only once per invokation
checkHash :: Ord k => Shell k -> Hash k -> Bool
checkHash y = (`listSubset` calcHash y)

listSubset [] _ = True
listSubset ((x1,x2):xs) ((y1,y2):ys) =
    case compare x1 y1 of
        EQ -> x2 <= y2 && listSubset xs ys
        LT -> False
        GT -> listSubset ((x1,x2):xs) ys
listSubset _ _ = False

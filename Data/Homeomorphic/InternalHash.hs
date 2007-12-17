
module Data.Homeomorphic.InternalHash(
    module Data.Homeomorphic.InternalHash,
    module Data.Homeomorphic.Internal
    ) where

import Data.Homeomorphic.Internal
import Data.Maybe
import Data.List(foldl')

data HomeomorphicWith k v h = Homeomorphic [(Shell k, h, v)]


emptyWith :: HomeomorphicWith k v h
emptyWith = Homeomorphic []

insertWith :: Ord k => (Shell k -> h) -> Shell k -> v -> HomeomorphicWith k v h -> HomeomorphicWith k v h
insertWith calcHash k v (Homeomorphic xs) = Homeomorphic ((k,calcHash k,v):xs)

findWith :: Ord k => (Shell k -> h -> Bool) -> Shell k -> HomeomorphicWith k v h -> [v]
findWith checkHash k (Homeomorphic xs) = [c | (a,b,c) <- xs, check b, a <<| k]
    where check = checkHash k

findOneWith :: Ord k => (Shell k -> h -> Bool) -> Shell k -> HomeomorphicWith k v h -> Maybe v
findOneWith checkHash k = listToMaybe . findWith checkHash k

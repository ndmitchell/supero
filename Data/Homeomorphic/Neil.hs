
module Data.Homeomorphic.Neil where

import qualified Data.Map as Map
import Data.Homeomorphic.Internal

data Homeomorphic k v = Homeomorphic [v] (Map.Map (Int,k) (Homeomorphic k v))


empty :: Homeomorphic k v
empty = Homeomorphic [] Map.empty


insert :: Ord k => Shell k -> v -> Homeomorphic k v -> Homeomorphic k v
insert k v = add (flatten k)
    where
        flatten (Shell a b c) = (b,a) : concatMap flatten c

        add []     (Homeomorphic a b) = Homeomorphic (v:a) b
        add (x:xs) (Homeomorphic a b) = Homeomorphic a b2
            where
                b2 = Map.insertWith comb x (add xs empty) b
                comb new old = add xs old


find :: Ord k => Shell k -> Homeomorphic k v -> [v]
find k = match [k]
    where
        match []     (Homeomorphic ans _ ) = ans
        match (k:ks) (Homeomorphic _   mp) = concatMap f (shell k)
            where f (a,b) = maybe [] (match $ b++ks) $ Map.lookup a mp

        shell (Shell a b c) = ((b,a), c) : concatMap shell c

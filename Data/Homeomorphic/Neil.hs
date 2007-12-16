
module Data.Homeomorphic.Neil where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

import Data.Homeomorphic.Internal
import Data.List
import Debug.Trace

{-
Problem: a <<| b may happen twice, since there may be
two different dive/couple ways to end up with a.

Solution: Store Int's, lookup each only once.
-}

data Homeomorphic k v = Homeomorphic (IntMap.IntMap v) (H k)

data H k = H [Int] (Map.Map (Int,k) (H k))


empty :: Homeomorphic k v
empty = Homeomorphic IntMap.empty emptyH

emptyH = H [] Map.empty


insert :: Ord k => Shell k -> v -> Homeomorphic k v -> Homeomorphic k v
insert k v (Homeomorphic a b) = Homeomorphic (IntMap.insert i v a) (add (flatten k) b)
    where
        i = IntMap.size a
        flatten (Shell a b c) = (b,a) : concatMap flatten c

        add []     (H a b) = H (i:a) b
        add (x:xs) (H a b) = H a b2
            where
                b2 = Map.insertWith comb x (add xs emptyH) b
                comb new old = add xs old


find :: Ord k => Shell k -> Homeomorphic k v -> [v]
find k (Homeomorphic a b) = map (a IntMap.!) res
    where res = reverse $ sort $ nub $ findIds [k] b


findIds :: Ord k => [Shell k] -> H k -> [Int]
findIds []     (H ans _ ) = ans
findIds (k:ks) (H _   mp) = concatMap f (shell k)
    where
        f (a,b) = maybe [] (findIds $ b++ks) $ Map.lookup a mp

        shell (Shell a b c) = ((b,a), c) : concatMap shell c

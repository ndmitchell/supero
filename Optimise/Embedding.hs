{-
    Cannot fully optimise, but can make all up to the first
    couple operation much cheaper
-}

module Optimise.Embedding(Embedding, insert, lookup, term) where

import Prelude hiding (lookup)
import qualified Data.Map as Map



data Term a = Term a Int [Term a]

term :: a -> [Term a] -> Term a
term a as = Term a (length as) as

(<<|) :: Eq a => Term a -> Term a -> Bool
(<<|) x@(Term x1 xn xs) y@(Term y1 yn ys) =
    any (x <<|) ys ||
    (xn == yn && x1 == y1 && and (zipWith (<<|) xs ys))




newtype Embedding k v = Embedding (Map.Map (k,Int) [(v,[Term k])])

insert :: Ord k => Term k -> v -> Embedding k v -> Embedding k v
insert k v (Embedding x) = Embedding (f v x k)
    where
        f v x (Term k n ks) =
            foldl (f v) (Map.insertWith (++) (k,n) [(v,ks)] x) ks

lookup :: Ord k => Term k -> Embedding k v -> [v]
lookup (Term k n ks) (Embedding x) = [v
    | r <- Map.lookup (k,n) x
    , (v,ls) <- r
    , and $ zipWith (<<|) ks ls]


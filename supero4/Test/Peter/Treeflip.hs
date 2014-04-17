
module Test.Peter.Treeflip(test) where

import Prelude hiding (flip)

flip t = case t of
    (Leaf x) -> Leaf x
    (Branch l r) -> Branch (flip l) (flip r)

sumtr t = case t of
    Leaf x -> x
    Branch l r -> sumtr l + sumtr r

buildTree n t = case n == 0 of
    True -> t
    False -> buildTree (n-1) (Branch t t)

root :: Int -> Int
root n = sumtr (flip (flip (buildTree n (Leaf 1))))

#if MAIN
data Tree a = Leaf a | Branch (Tree a) (Tree a)

test = (\i -> root (i :: Int) :: Int, 22 :: Int)
#endif

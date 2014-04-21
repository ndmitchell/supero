
module Test.Peter.Raytracer(test) where

#include "Include.hs"

root xs ys = sum (zipWith (*) xs ys)

#if MAIN
test = (\n -> root (replicate n 1) (replicate n 2) :: Int, 1000000 :: Int)
#endif

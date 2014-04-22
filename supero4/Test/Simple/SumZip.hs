
module Test.Simple.SumZip(test) where

#include "Include.hs"

#if MAIN
test = (\n -> root (n :: Int) :: Int, 1000000 :: Int)
#endif

root n = sum [x + y | (x,y) <- zip [1..n] [100..]]


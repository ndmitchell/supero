
module Test.Nofib.X2n1(test) where

#if MAIN
import Data.Complex

test = (\i -> root (i :: Int) :: Int, 80000 :: Int)
#endif

#include "Include.hs"

f :: Int -> Complex Double
f n = mkPolar 1 ((2*pi)/fromIntegral n) ^ n

root n = round (realPart (sum [f n | n <- [1 .. n]]))

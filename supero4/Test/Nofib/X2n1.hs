
module Test.Nofib.X2n1(test) where

#if MAIN
import Data.Complex

test = (\i -> root (i :: Int) :: Int, 80000 :: Int)

operation :: Int -> Complex Double
operation n = mkPolar 1 ((2*pi)/fromIntegral n) ^ n
#endif

#include "Include.hs"

root n = round (realPart (sum (map operation (enumFromTo 1 n))))

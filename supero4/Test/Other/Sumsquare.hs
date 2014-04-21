
module Test.Other.Sumsquare(test) where

#include "Include.hs"

f :: Int -> Int
-- f n = sum [ k * m | k <- [1..n], m <- [1..k] ]
f n = sum (concatMap (\k -> map (\m -> k * m) (enumFromTo 1 k)) (enumFromTo 1 n))

root x = f x

#if MAIN
test = (\i -> root i :: Int, 1000 :: Int)
#endif


module Test.Simple.SumZip2(test) where

#include "Include.hs"

#if MAIN
test = (\n -> root [100..] (n :: Int) :: Int, 1000000 :: Int)

{-# NOINLINE root #-}
root :: [Int] -> Int -> Int

#endif

root xs n = sum [x + y | (x,y) <- zip [1..n] xs]

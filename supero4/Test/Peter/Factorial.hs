
module Test.Peter.Factorial(test) where

#include "Include.hs"

fac :: Int -> Int
fac n = case n == 0 of
    True -> 1
    False -> n * fac (jail (n-1))

root x = fac x

#if MAIN
test = (\i -> root i :: Int, 1000000 :: Int)
#endif

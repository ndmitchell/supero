
module Test.Nofib.Primes(test) where

#include "Include.h"

suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter ns = case ns of
    (n:ns) -> filter (isdivs n) ns
    [] -> error "the_filter"

primes :: [Int]
primes = map head (iterate the_filter (iterate suCC 2))

root x = primes !! x

#if MAIN
main = (\i -> root (i :: Int) :: Int, 4000 :: Int)
#endif

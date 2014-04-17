
module Test.Simple.Evens(test) where

#include "Include.h"

isdivs :: Int -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter ns = case ns of
    (n:ns) -> filter (isdivs n) ns
    [] -> error "the_filter"

root i = the_filter (iterate succ 2) !! i

#if MAIN
test = (\i -> root (i :: Int) :: Int, 1000 :: Int)
#endif

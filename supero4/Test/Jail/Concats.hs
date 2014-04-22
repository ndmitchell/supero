-- !!! count the number of solutions to the "n queens" problem.
-- (grabbed from LML dist)

module Test.Jail.Concats(test) where

#include "Include.hs"

#if MAIN
test = (\i -> root (i :: Int) :: [[Int]], 3 :: Int)
#endif


root :: Int -> [[Int]]
root nq = gen nq nq

gen :: Int -> Int -> [[Int]]
gen nq n = case n == 0 of
    True -> [[]]
    False -> map id (jail (gen nq (n-1)))

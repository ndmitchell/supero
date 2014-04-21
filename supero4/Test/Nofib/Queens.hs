-- !!! count the number of solutions to the "n queens" problem.
-- (grabbed from LML dist)

module Test.Nofib.Queens(test) where

#include "Include.hs"

#if MAIN
test = (\i -> root (i :: Int) :: Int, 12 :: Int)

safer'3 x d q = x /= q && x /= q+d && x /= q-d 
#endif


root :: Int -> Int
root nq = length (gen nq nq)

safe :: Int -> Int -> [Int] -> Bool
safe x d q = case q of
    [] -> True
    q:l -> safer'3 x q d {- x /= q && x /= q+d && x /= q-d -} && safe x (jail d+1) l

gen :: Int -> Int -> [[Int]]
gen nq n = case n == 0 of
    True -> [[]]
    False -> 
        --[ (q:b) | b <- gen nq (n-1), q <- [1..nq], safe q 1 b]
        concatMap (\b -> concatMap (\q -> if safe q 1 b then [q:b] else []) (enumFromTo 1 nq)) (jail (gen nq (jail n-1)))

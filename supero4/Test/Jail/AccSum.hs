
module Test.Jail.AccSum(test) where

#if MAIN
test = (root (0 :: Int), [1,2,3] :: [Int])
#endif

#include "Include.hs"

root acc n = sumWith_ acc n

sumWith_ acc x = case x of
    [] -> acc
    x:xs -> sumWith_ (x + jail acc) xs


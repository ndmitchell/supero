
module Test where

{-
main :: Int -> Int -> Int
main n m = len [j | i <- enum [1..n], j <- [1..m]]

len (x:xs) = 1 + len xs
len [] = 0

main n = len (enum 1 n)


enum :: Int -> Int -> [Int]
enum i n = if i > n then [] else i : enum (i+1) n


len :: [a] -> Int
len (x:y:xs) = 2 + len xs
len (x:xs) = 1 + len xs
len [] = 0


-}

{-
main :: Int -> [()]
main 0 = []
main n = [() | b <- main (n-1), q <- [1]]
-}

main :: Int -> [()]
main 0 = []
main n = let ok2 q = [()]
             ok1 p = concatMap ok2 [(1::Int)]
         in concatMap ok1 (main (n-(1::Int)))


module Main where

{- 
suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

eq x = x == 0
sub x = x - 1
-}

isdivs x y = prim (IsDivs x y)


the_filter :: [Int] -> [Int]
the_filter ns = case ns of
    (n:ns) -> filter (isdivs n) ns
    [] -> error "the_filter"

primes :: [Int]
primes = map head (iterate the_filter (iterate suCC 2))

main x = primes !! x


-- LIBRARY STUFF --

head x = case x of
    [] -> error "head"
    x:xs -> x

map f x = case x of
    y:ys -> f y : map f ys
    [] -> []

filter f x = case x of
    [] -> []
    x:xs -> case f x of
        True -> x : filter f xs
        False -> filter f xs

iterate f x = x : iterate f (f x)

(!!) :: [a] -> Int -> a
(!!) xs y = case xs of
    [] -> error "bad"
    x:xs -> case eq y of
        True -> x
        False -> (!!) xs (sub y)

    
    

module Main where

--isdivs :: Int -> Int -> Bool
--isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter ns = case ns of
    (n:ns) -> filter (isdivs n) ns
    [] -> error "the_filter"

evens :: [Int]
evens = the_filter (iterate suCC 2)

main x = evens !! x


filter f x = case x of
    [] -> []
    x:xs -> case f x of
        True -> x : filter f xs
        False -> filter f xs

iterate f x = x : iterate f (jail (f x))

(!!) :: [a] -> Int -> a
(!!) xs y = case xs of
    [] -> error "bad"
    x:xs -> case eq y of
        True -> x
        False -> (!!) xs (jail (sub y))


module Test.Simple.Evens(test) where

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

#if SUPERO
succ x = x + 1

filter f x = case x of
    [] -> []
    x:xs -> case f x of
        True -> x : filter f xs
        False -> filter f xs

iterate f x = x : iterate f (jail (f x))

(!!) :: [a] -> Int -> a
(!!) xs y = case xs of
    [] -> error "bad"
    x:xs -> case y == 0 of
        True -> x
        False -> (!!) xs (jail (y - 1))
#endif

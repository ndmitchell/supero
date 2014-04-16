
module Main(main) where

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

main = print (root (4000 :: Int) :: Int)

#endif

#if MAIN_SUPERO

addInt'2 = (+)
eqInt'2 = (==)
neqInt'2 = (/=)
modInt'2 = mod
subInt'2 = (-)
error'1 = error

#endif

#if SUPERO

(+) = addInt'2
(==) = eqInt'2
(/=) = neqInt'2
mod = modInt'2
(-) = subInt'2
error = error'1

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

iterate f x = x : iterate f (jail (f x))

(!!) :: [a] -> Int -> a
(!!) xs y = case xs of
    [] -> error "bad"
    x:xs -> case y == 0 of
        True -> x
        False -> (!!) xs (y - 1)

#endif

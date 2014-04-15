
import Prelude hiding(zipWith, sum)

zipWith f xs ys = case xs of
    [] -> []
    x:xs -> case ys of
        [] -> []
        y:ys -> f x y : zipWith f xs ys

sum xs = sumWith 0 xs

sumWith acc xs = case xs of
    [] -> acc
    x:xs -> sumWith (x+acc) xs

root xs ys = sum (zipWith (*) xs ys)

#if MAIN
main = print (root (replicate n 1) (replicate n 2) :: Int)
    where n = 1000000

eq'2 = (==) :: Int -> Int -> Bool
add'2 = (+) :: Int -> Int -> Int
sub'2 = (-) :: Int -> Int -> Int
mul'2 = (*) :: Int -> Int -> Int
#endif

#if SUPERO
(==) = eq'2
(+) = add'2
(*) = mul'2
(-) = sub'2
#endif


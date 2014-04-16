

f :: Int -> Int
-- f n = sum [ k * m | k <- [1..n], m <- [1..k] ]
f n = sum (concatMap (\k -> map (\m -> k * m) (enumFromTo 1 k)) (enumFromTo 1 n))

root x = f x

#if MAIN
main = print $ root (10000 :: Int)

eq'2 = (==) :: Int -> Int -> Bool
gt'2 = (>) :: Int -> Int -> Bool
add'2 = (+) :: Int -> Int -> Int
sub'2 = (-) :: Int -> Int -> Int
mul'2 = (*) :: Int -> Int -> Int
#endif

#if SUPERO
(==) = eq'2
(+) = add'2
(*) = mul'2
(-) = sub'2
(>) = gt'2

sum xs = sumWith 0 xs

sumWith acc xs = case xs of
    [] -> jail acc
    x:xs -> sumWith (x+ jail acc) xs

enumFromTo i j = if i > j then [] else i : enumFromTo (i+1) j

concatMap f x = case x of
    [] -> []
    x:xs -> f x ++ concatMap f xs

(++) xs ys = case xs of
    [] -> ys
    x:xs -> x : (xs ++ ys)

map f x = case x of
    y:ys -> f y : map f ys
    [] -> []


#endif


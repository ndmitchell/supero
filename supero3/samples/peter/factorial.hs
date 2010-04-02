
fac :: Int -> Int
fac n = case n == 0 of
    True -> 1
    False -> n * fac (n-1)

root x = fac x


#if MAIN
main = print $ (root (1000000 :: Int) :: Int)

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

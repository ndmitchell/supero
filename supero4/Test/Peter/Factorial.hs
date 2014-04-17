
module Test.Peter.Factorial(test) where

fac :: Int -> Int
fac n = case n == 0 of
    True -> 1
    False -> n * fac (jail (n-1))

root x = fac x

#if MAIN
test = (\i -> root i :: Int, 1000000 :: Int)

jail = id

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

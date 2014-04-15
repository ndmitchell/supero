

#if MAIN
import Complex

main = print $ root ( 80000 :: Int)

f'1 :: Int -> Complex Double
f'1 n = mkPolar 1 ((2*pi)/fromIntegral n) ^ n

round'1 = round
realPart'1 = realPart
compAdd'2 = (+) :: Complex Double -> Complex Double -> Complex Double
intAdd'2 = (+) :: Int -> Int -> Int
intGt'2 = (>) :: Int -> Int -> Bool

#endif

root n = round (realPart (sum (map f'1 (enumFromTo 1 n))))


#if SUPERO

round = round'1
realPart = realPart'1

(+) = intAdd'2
(>) = intGt'2

map f x = case x of
    y:ys -> f y : map f ys
    [] -> []

sum xs = case xs of
    [] -> 0
    x:xs -> sum2 x xs
sum2 x xs = case xs of
    [] -> x
    y:ys -> sum2 (y `compAdd'2` x) ys


enumFromTo from to = case from > to of
    True -> []
    False -> from : enumFromTo (from `intAdd'2` 1) to

#endif

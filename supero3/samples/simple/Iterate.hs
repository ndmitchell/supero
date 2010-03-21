
module Main(main) where

#if STREAM
import Prelude hiding((!!), iterate, map)
import Data.List.Stream

#endif

#if MAIN

main = print (root (400000 :: Int) :: Int)

#endif


root n = map square (iterate inc 1) !! n
inc x = x + 1
square x = x * x



#if MAIN_SUPERO

addInt'2 = (+)
mulInt'2 = (*)
eqInt'2 = (==)
neqInt'2 = (/=) :: Int -> Int -> Bool
modInt'2 = mod
subInt'2 = (-)
error'1 = error

#endif

#if SUPERO

(+) = addInt'2
(*) = mulInt'2
(==) = eqInt'2
(/=) = neqInt'2
mod = modInt'2
(-) = subInt'2
error = error'1


map f x = case x of
    y:ys -> f y : map f ys
    [] -> []

iterate f x = x : iterate f (f x)

(!!) :: [a] -> Int -> a
(!!) xs y = case xs of
    [] -> error "bad"
    x:xs -> case y == 0 of
        True -> x
        False -> (!!) xs (y - 1)

#endif

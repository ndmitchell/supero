
module Main where


main n = {- map square -} (repeat 0) !! n

{-
square x = x * x
inc x = x + 1
eq x = x == 0
-}
dec x = x - 1
eq x = prim (Equal x 0)

repeat x = x : repeat x

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
        False -> (!!) xs (dec y)

    
    
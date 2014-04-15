
module Main where


main x = map id x

map f x = case x of
    [] -> []
    y:ys -> f y : map f ys

id x = x

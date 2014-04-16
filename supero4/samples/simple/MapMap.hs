
module Main where


root f g x = map f (map g x)

map f x = case x of
    [] -> []
    y:ys -> f y : map f ys

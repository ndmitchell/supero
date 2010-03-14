
module Main where

main x n = x !! n


(!!) :: [a] -> Int -> a
(!!) xs y = case xs of
    [] -> error "bad"
    x:zs -> case y == 0 of
        True -> x
        False -> (!!) zs (y-1)

    
    
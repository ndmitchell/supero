
module Main where

main xs = rev [] xs
rev acc xs = case  xs of
                   []    -> acc
                   y:ys  -> rev (y:acc) ys

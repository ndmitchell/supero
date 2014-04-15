
module Main where

root xs = rev [] xs
rev acc xs = case  xs of
                   []    -> acc
                   y:ys  -> rev (y:acc) ys

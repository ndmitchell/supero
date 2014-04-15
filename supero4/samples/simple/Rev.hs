
module Main where

root xs = rev [] xs
rev acc xs = case  xs of
                   []    -> acc
                   y:ys  -> rev (jail (y:acc)) ys

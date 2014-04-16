
module Main where

root xs = rev [] xs
rev acc xs = case  xs of
                   []    -> jail acc
                   y:ys  -> rev (y:jail acc) ys

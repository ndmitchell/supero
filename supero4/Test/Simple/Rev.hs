
module Test.Simple.Rev(main) where

#if MAIN
main = print $ root "neil"
jail = id
#endif

root xs = rev [] xs
rev acc xs = case  xs of
                   []    -> jail acc
                   y:ys  -> rev (y:jail acc) ys

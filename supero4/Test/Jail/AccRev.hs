
module Test.Jail.AccRev(test) where

#include "Include.h"

#if MAIN
test = (uncurry root, ("test", "more"))
#endif

root xs ys = rev xs ys
rev acc xs = case  xs of
                   []    ->  acc
                   y:ys  -> rev (y: jail acc) ys

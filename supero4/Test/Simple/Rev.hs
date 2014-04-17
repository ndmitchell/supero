
module Test.Simple.Rev(test) where

#include "Include.h"

#if MAIN
test = (root, "neil")
#endif

root xs = rev [] xs
rev acc xs = case  xs of
                   []    -> jail acc
                   y:ys  -> rev (y:jail acc) ys

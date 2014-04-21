
module Test.Simple.MapId(test) where

#include "Include.hs"

#if MAIN
test = (root, "test")
#endif

root x = map id x

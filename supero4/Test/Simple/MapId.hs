
module Test.Simple.MapId(test) where

#include "Include.h"

#if MAIN
test = (root, "test")
#endif

root x = map id x


module Test.Simple.Index(test) where

#include "Include.h"

#if MAIN
test = (\(s,i) -> root s (i::Int), ("test", 2::Int))
#endif

root x n = x !! n

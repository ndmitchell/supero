
module Test.Simple.Iterate(test) where

#include "Include.h"

#if STREAM
import Prelude hiding((!!), iterate, map)
import Data.List.Stream
#endif

#if MAIN
test = (\i -> root (i :: Int) :: Int, 400000 :: Int)
#endif

root n = map square (iterate inc 1) !! n
inc x = x + 1
square x = x * x

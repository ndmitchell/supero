
module Test.Simple.MapMap(test) where

#include "Include.h"

#if MAIN
test = (\(a,b,c) -> root a b c, ((+1),(*2),[1,2,3]))
#endif

root f g x = map f (map g x)

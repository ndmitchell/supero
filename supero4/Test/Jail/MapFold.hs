
module Test.Jail.MapFold(test) where

#include "Include.hs"

#if MAIN
test = (\(a,b) -> root a b, ((+1),[1,2,3]))
#endif

root f xs = foldr (\a b -> (:) (f a) $ map id b) [] xs

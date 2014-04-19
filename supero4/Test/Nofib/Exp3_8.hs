
module Test.Nofib.Exp3_8(test) where

#include "Include.h"

x +& y = case x of
    Z -> y
    S x -> S (x +& y)

x *& y = case y of
    Z -> Z
    S y -> jail (x *& y) +& x


fromInteger_ x = if x < 1 then Z else S (fromInteger_ (x-1))

int :: Nat -> Int
int x = case x of
    Z     -> 0
    (S x) -> 1 + int x

x ^^^ y = case y of
    Z   -> S Z
    S y -> x *& jail (x ^^^ y)

root n = int (fromInteger_ 3 ^^^ fromInteger_ n)


#if MAIN
data Nat = Z | S Nat

test = (\i -> root (i :: Int) :: Int, 7 :: Int)
#endif

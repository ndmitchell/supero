
module Test.Jail.AddMul(test) where

#include "Include.hs"

#if MAIN
data Nat = Z | S Nat deriving (Read,Show)
test = (\x -> show $ root (read x) (read x), "S (S Z)")
#endif

add x y = case x of
    Z -> y
    S x -> S (add x y)

mul x y = case y of
    Z -> Z
    S y -> jail (mul x y) `add` x

root x y = mul x y

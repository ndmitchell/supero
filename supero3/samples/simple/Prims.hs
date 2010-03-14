
module Main where

main x y z = if z == 0 then x * y else (x+y) * z

(==) x y = prim (EqInt x y)
(*) x y = prim (MulInt x y)
(+) x y = prim (AddInt x y)


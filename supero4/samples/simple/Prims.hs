
module Main where

root x y z = if z == 0 then x * y else (x+y) * z

(==) x y = primEqInt'2 x y
(*) = primMulInt'2
(+) = primAddInt'2



module Test.Nofib.Tak(test) where

#include "Include.hs"

tak :: Int -> Int -> Int -> Int
tak x y z = if not(y < x) then z
       else tak (tak (x-1) y z)
		(tak (y-1) z x)
		(tak (z-1) x y)

root x y z = tak x y z

#if MAIN
test = (\(x,y,z) -> root (x::Int) (y::Int) (z::Int) :: Int, (24::Int,16::Int,8::Int))
#endif

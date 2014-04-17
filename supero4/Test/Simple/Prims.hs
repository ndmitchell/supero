
module Test.Simple.Prims(test) where

#if MAIN
test = (\(a,b,c) -> root (a::Int) (b::Int) (c::Int), (8::Int,7::Int,12::Int))
#endif

root x y z = if z == 0 then x * y else (x+y) * z

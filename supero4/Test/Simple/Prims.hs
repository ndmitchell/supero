
module Test.Simple.Prims(test) where

#if MAIN
test = (\(a,b,c) -> root (a::Int) (b::Int) (c::Int), (8::Int,7::Int,12::Int))
#endif

root x y z = if z == 0 then x * y else (x+y) * z

#if SUPERO
(==) x y = eqInt'2 x y
(*) = mulInt'2
(+) = addInt'2
#endif

#if MAIN_SUPERO
addInt'2 = (+)
eqInt'2 = (==)
mulInt'2 = (*)
#endif

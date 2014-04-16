
module Test.Simple.Prims(main) where

#if MAIN
main = print $ root 8 7 12
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

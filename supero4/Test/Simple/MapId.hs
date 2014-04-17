
module Test.Simple.MapId(test) where

#if MAIN
test = (root, "test")
#endif

root x = map id x

#if SUPERO
map f x = case x of
    [] -> []
    y:ys -> f y : map f ys

id x = x
#endif

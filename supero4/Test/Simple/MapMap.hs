
module Test.Simple.MapMap(main) where

#if MAIN
main = print $ root (+1) (*2) [1,2,3]
#endif

root f g x = map f (map g x)

#if SUPERO
map f x = case x of
    [] -> []
    y:ys -> f y : map f ys
#endif

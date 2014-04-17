#if SUPERO

map f x = case x of
    [] -> []
    y:ys -> f y : map f ys


#endif

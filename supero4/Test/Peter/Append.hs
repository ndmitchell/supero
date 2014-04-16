

#if MAIN

main = print $ length $ root (replicate n 'x') (replicate n 'y') (replicate n 'z')
    where n = 10000000

#endif

app xs ys = case xs of
    [] -> ys
    x:xs -> x : app xs ys

root xs ys zs = app (app xs ys) zs

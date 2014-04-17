
module Test.Peter.Append(test) where

#if MAIN
test = (\n -> length $ root (replicate n 'x') (replicate n 'y') (replicate n 'z'), 10000000 :: Int)
#endif

app xs ys = case xs of
    [] -> ys
    x:xs -> x : app xs ys

root xs ys zs = app (app xs ys) zs

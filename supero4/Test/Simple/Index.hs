
module Test.Simple.Index(test) where

#if MAIN
test = (\(s,i) -> root s (i::Int), ("test", 2::Int))
#endif

root x n = x !! n

#if SUPERO
(!!) :: [a] -> Int -> a
(!!) xs y = case xs of
    [] -> error "bad"
    x:zs -> case y == 0 of
        True -> x
        False -> (!!) zs (jail (y-1))
#endif


module Test.Simple.Index(main) where

#if MAIN
main = print $ root "test" 2
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

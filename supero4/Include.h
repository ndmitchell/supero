#if SUPERO

map f x = case x of
    [] -> []
    y:ys -> f y : map f ys

id x = x

iterate f x = x : iterate f (jail (f x))

(!!) :: [a] -> Int -> a
(!!) xs y = case xs of
    [] -> error "bad"
    x:xs -> case y == 0 of
        True -> x
        False -> (!!) xs (jail (y - 1))

succ x = x + 1

filter f x = case x of
    [] -> []
    x:xs -> if f x then x : filter f xs else filter f xs

zipWith f xs ys = case xs of
    [] -> []
    x:xs -> case ys of
        [] -> []
        y:ys -> f x y : zipWith f xs ys

sum xs = sumWith 0 xs

sumWith acc xs = case xs of
    [] -> jail acc
    x:xs -> sumWith (x + jail acc) xs

#else

{-# INLINE jail #-}
jail x = x

#endif

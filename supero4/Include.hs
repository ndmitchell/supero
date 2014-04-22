#if SUPERO

map f x = case x of
    [] -> []
    y:ys -> f y : map f ys

id x = x

iterate f x = x : iterate f (jail (f x))

array lu ies = case lu of
    (l,u) -> let n = safeRangeSize (l,u)
             in unsafeArray' (l,u) n
                     [(safeIndex (l,u) n i, e) | (i, e) <- ies]

(!!) :: [a] -> Int -> a
(!!) xs y = case xs of
    [] -> error "bad"
    x:xs -> case y == 0 of
        True -> x
        False -> (!!) xs (jail y - 1)

filter f x = case x of
    [] -> []
    x:xs -> if f x then x : filter f xs else filter f xs

zipWith f xs ys = case xs of
    [] -> []
    x:xs -> case ys of
        [] -> []
        y:ys -> f x y : zipWith f xs ys

sum xs = sumWith 0 xs
sumWith acc x = case x of
    [] -> jail acc
    x:xs -> sumWith (x + jail acc) xs

head x = case x of
    [] -> error "head"
    x:xs -> x

tail x = case x of
    [] -> error "tail"
    x:xs -> xs

concat x = case x of
    [] -> []
    x:xs -> x ++ concat xs

(++) xs ys = case xs of
    [] -> ys
    x:xs -> x : (xs ++ ys)

take :: Int -> [a] -> [a]
take n x = case n == 0 of
    True -> []
    False -> case x of
        [] -> []
        x:xs -> x : take (n-1) xs

repeat x = x : repeat x

not x = case x of
    True -> False
    False -> True

($) f x = f x

(.) f g x = f (g x)


enumFrom x = x : enumFrom (succ x)

enumFromTo from to = case from > to of
    True -> []
    False -> from : enumFromTo (succ (jail from)) to

a || b = case a of
    True -> True
    False -> b

a && b = case a of
    True -> b
    False -> False

length x = lengthWith 0 x
lengthWith acc x = case x of
    [] -> jail acc
    x:xs -> lengthWith (jail acc + 1) xs

reverse xs = reverseWith [] xs
reverseWith acc xs = case xs of
    [] -> jail acc
    x:xs -> reverseWith (x:jail acc) xs


concatMap f x = case x of
    [] -> []
    x:xs -> f x ++ jail (concatMap f xs)

mapMaybe f x = case x of
    [] -> []
    x:xs -> case f x of
        Nothing -> mapMaybe f xs
        Just y -> y : mapMaybe f xs

#else

{-# INLINE jail #-}
jail x = x

#endif


module Example10 where

main x = case f x of
            [] -> []
            (x:xs) -> x:xs


f xs = case xs of
        [] -> []
        (x:xs) -> True : mapid (f xs)



mapid xs = case xs of
        [] -> []
        (x:xs) -> x : mapid xs

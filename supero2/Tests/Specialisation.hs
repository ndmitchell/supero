

map f x = case x of
    [] -> []
    x:xs -> f x : map f xs

main = map inc

inc x = x + 1



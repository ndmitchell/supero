

(.) f g x = f (g x)

map f x = case x of
    [] -> []
    x:xs -> f x : map f xs

main f g = map f . map g


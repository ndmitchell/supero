
range i n = case i <= n of
                True -> i : range (i+1) n
                False -> []

sum x = case x of
    [] -> 0
    x:xs -> x + sum xs

main n = sum (range 0 n)

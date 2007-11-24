
module Main where


main = main_small `seq` putChar 'x'

main_small x = list (list x)

list x = case x of
             [] -> []
             (x:xs) -> x : list xs




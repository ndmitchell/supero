
module Main where


main = main_small `seq` putChar 'x'

main_small x = map (+ (1::Int)) (map (+2) x)

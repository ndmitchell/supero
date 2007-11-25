
module Main where


main = main_small `seq` putChar 'x'

main_small x = takeWhile (== (0::Int)) x




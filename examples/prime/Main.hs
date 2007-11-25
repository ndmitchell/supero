
module Main where


main = main_small `seq` putChar 'x'

main_small step start = map head (iterate step start)

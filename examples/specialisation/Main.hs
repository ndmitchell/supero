
module Main where


main = main_small `seq` putChar 'x'

main_small x = map head x



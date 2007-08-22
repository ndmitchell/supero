
{-
main :: Int -> Int -> Int
main n m = len [j | i <- enum [1..n], j <- [1..m]]

len (x:xs) = 1 + len xs
len [] = 0

main n = len (enum 1 n)


enum :: Int -> Int -> [Int]
enum i n = if i > n then [] else i : enum (i+1) n


len :: [a] -> Int
len (x:y:xs) = 2 + len xs
len (x:xs) = 1 + len xs
len [] = 0


-}

{-
main :: Int -> [()]
main 0 = []
main n = [() | b <- main (n-1), q <- [1]]
-}

{-
main :: Int -> [[()]]
main 0 = []
main n = mapnil (main (n-(1::Int)))


nil _ = []


mapnil x = case x of
                [] -> []
                (x:xs) -> [] : mapnil xs
-}

{-
import System

main :: IO ()
main = do
    [x] <- getArgs
    print $ sum [1 .. read x :: Int]
-}

module Test where

main :: [Int] -> Int
main xs = foldl (+) 0 xs

enum :: Int -> Int -> [Int]
enum i n = if i > n then [] else i : enum (i+1) n

-- !!! Wentworth's version of a program to generate
-- !!! all the expansions of a generalised regular expression
-- !!!
--
-- RJE: Modified so it only outputs the number of characters in the output, 
-- rather that the output itself, thus avoiding having to generate such a 
-- huge output file to get a reasonable execution time.

module Main (main) where

import Char

numchars :: [String] -> Int
numchars l = sum $ map length l

expand ys = case ys of
    [] -> [[]]
    x:xs -> case x == '<' of
        True -> numericRule xs
        False -> case x == '[' of
            True -> alphabeticRule xs
            False -> constantRule ys

constantRule xs = case xs of
    c:rest -> map ((:) c) (expand rest)
    [] -> error "constantRule"

alphabeticRule xs = case xs of
    [] -> error "alpha"
    a:xs -> case xs of
        [] -> error "alpha"
        x:xs -> case x == '-' of
            False -> error "alpha"
            True -> case xs of
                [] -> error "alpha"
                b:xs -> case xs of
                    [] -> error "alpha"
                    x:rest -> case x == ']' of
                        False -> error "alpha"
                        True -> case a <= b of
                            True -> power (:) (enumFromTo a b) (expand rest)
                            False -> power (:) (reverse (enumFromTo b a)) (expand rest)


power f xs ys = concatMap (\x -> map (\y -> f x y) ys) xs


numericRule x = []
{-
  = [ pad (show i) ++ z
	| i <- if u < v then [u..v] else [u,u-1..v]
	, z <- expand s ]
  where
    (p,_:q) = span (/= '-') x
    (r,_:s) = span (/= '>') q
    (u,v)   = (mknum p, mknum r)
    mknum s = foldl (\ u c -> u * 10 + (ord c - ord '0')) 0 s
    pad s   = [ '0' | i <- [1 .. (width-(length s))]] ++ s
    width   = max (length (show u)) (length (show v))
-}

root x = numchars (expand x)

#if MAIN
main = print $ root "[a-j][a-j][a-j]abcdefghijklmnopqrstuvwxyz"
error'1 = Prelude.error
eqChar'2 = (==) :: Char -> Char -> Bool
gtChar'2 = (>) :: Char -> Char -> Bool
ltEqChar'2 = (<=) :: Char -> Char -> Bool
addInt'2 = (+) :: Int -> Int -> Int
incChar'2 = succ :: Char -> Char

#endif

#if SUPERO
error = error'1
(==) = eqChar'2
(>) = gtChar'2
($) f x = f x
(<=) = ltEqChar'2
(+) = addInt'2

map f x = case x of
    y:ys -> f y : map f ys
    [] -> []

concatMap f x = concat (map f x)

concat x = case x of
    [] -> []
    x:xs -> x ++ concat xs

(++) xs ys = case xs of
    [] -> ys
    x:xs -> x : (xs ++ ys)

enumFromTo from to = case from > to of
    True -> []
    False -> from : enumFromTo (incChar'1 from) to

sum x = sumWith 0 x
sumWith acc x = case x of
    [] -> acc
    x:xs -> sumWith (acc+x) xs

length x = lengthWith 0 x
lengthWith acc x = case x of
    [] -> acc
    x:xs -> lengthWith (acc+1) xs

reverse xs = reverseWith [] xs
reverseWith acc xs = case xs of
    [] -> acc
    x:xs -> reverseWith (x:acc) xs

#endif


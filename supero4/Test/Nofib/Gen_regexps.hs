-- !!! Wentworth's version of a program to generate
-- !!! all the expansions of a generalised regular expression
-- !!!
--
-- RJE: Modified so it only outputs the number of characters in the output, 
-- rather that the output itself, thus avoiding having to generate such a 
-- huge output file to get a reasonable execution time.

module Test.Nofib.Gen_regexps(test) where

import Data.Char
#include "Include.h"

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
test = (root, "[a-j][a-j][a-j]abcdefghijklmnopqrstuvwxyz")
#endif


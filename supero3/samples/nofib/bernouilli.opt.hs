{-# OPTIONS_GHC -O2 #-}
module Main(main) where
import Ratio

intToInteger'1 = fromIntegral :: Int -> Integer
eqInt'2 = (==) :: Int -> Int -> Bool
mulInteger'2 = (*) :: Integer -> Integer -> Integer
negateInteger'1 = negate :: Integer -> Integer
addInteger'2 = (+) :: Integer -> Integer -> Integer
subInt'2 = (-) :: Int -> Int -> Int
addInt'2 = (+) :: Int -> Int -> Int
modInt'2 = mod :: Int -> Int -> Int
addRational'2 = (+) :: Rational -> Rational -> Rational
gtInt'2 = (>) :: Int -> Int -> Bool
subInteger'2 = (-) :: Integer -> Integer -> Integer
rational'2 = (Ratio.%)
error'1 = Prelude.error

main = print $ root
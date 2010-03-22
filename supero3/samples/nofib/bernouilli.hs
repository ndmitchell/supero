
-- There was a lot of discussion about various ways of computing
-- Bernouilli numbers (whatever they are) on haskell-cafe in March 2003
-- Here's one of the programs.

-- It's not a very good test, I suspect, because it manipulates big integers,
-- and so probably spends most of its time in GMP.  

import Prelude hiding ((!!),map,filter,odd,enumFromTo,error,zipWith,enumFrom,(++),iterate,($),tail,sum,not,head)
import qualified Prelude
import Ratio hiding ((%))
import qualified Ratio

-- powers = [[r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
powers :: [[Integer]]
powers = enumFrom 2 : map (zipWith (mulInteger'2) (head powers)) powers

-- powers = [[(-1)^r * r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
neg_powers :: [[Integer]]
neg_powers = 
  map (zipWith (\n x -> if n then x else negateInteger'1 x) (iterate not True)) powers

pascal:: [[Integer]]
pascal = (1:2:1:[]) : map (\line -> zipWith (addInteger'2) (line++[0]) (0:line)) pascal

bernoulli :: Int -> Rational
bernoulli n = case eqInt'2 n 0 of
    True -> 1%1
    False -> case eqInt'2 n 1 of
        True -> (-1)%2
        False -> case odd n of
            True -> 0
            False -> let ps = neg_powers !! (subInt'2 n 1)
                     in ((-1)%2) `addRational'2` sumRational (zipWith (f ps) (enumFromTo 2 n) pascal)

f :: [Integer] -> Int -> [Integer] -> Rational
f powers k combs = ((sumInteger $ zipWith (mulInteger'2) powers (tail $ tail combs)) `subInteger'2` 
                            intToInteger'1 k) % intToInteger'1 (addInt'2 k 1)


root x = bernoulli x

#if IMPORT_SUPERO
import Ratio
#endif

#if MAIN_SUPERO
#endif

#if MAIN

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

main = print $ root (500 :: Int)
#endif

#if SUPERO

(%) = rational'2
($) f x = f x
error = error'1

map f x = case x of
    y:ys -> f y : map f ys
    [] -> []

iterate f x = x : iterate f (f x)

head x = case x of
    [] -> error "head"
    x:xs -> x

not x = case x of
    True -> False
    False -> True

(++) xs ys = case xs of
    [] -> ys
    x:xs -> x : (xs ++ ys)

enumFrom x = x : enumFrom (x `addInteger'2` 1)

zipWith f x y = case x of
    [] -> []
    x:xs -> case y of
        [] -> []
        y:ys -> f x y : zipWith f xs ys

odd x = modInt'2 x 2 `eqInt'2` 1

sumInteger xs = case xs of
    [] -> 0
    x:xs -> sumInteger2 x xs
sumInteger2 x xs = case xs of
    [] -> x
    y:ys -> sumInteger2 (y `addInteger'2` x) ys

sumRational xs = case xs of
    [] -> 0
    x:xs -> sumRational2 x xs
sumRational2 x xs = case xs of
    [] -> x
    y:ys -> sumRational2 (y `addRational'2` x) ys

tail x = case x of
    [] -> error "tail"
    x:xs -> xs

(!!) :: [a] -> Int -> a
(!!) xs y = case xs of
    [] -> error "bad"
    x:xs -> case y `eqInt'2` 0 of
        True -> x
        False -> (!!) xs (y `subInt'2` 1)

enumFromTo from to = case from `gtInt'2` to of
    True -> []
    False -> from : enumFromTo (from `addInt'2` 1) to

#endif

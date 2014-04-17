
module Test.Nofib.Bernouilli(test) where

-- There was a lot of discussion about various ways of computing
-- Bernouilli numbers (whatever they are) on haskell-cafe in March 2003
-- Here's one of the programs.

-- It's not a very good test, I suspect, because it manipulates big integers,
-- and so probably spends most of its time in GMP.  

--import Prelude hiding ((!!),map,filter,odd,enumFromTo,error,zipWith,enumFrom,(++),iterate,($),tail,sum,not,head)
--import qualified Prelude
--import Ratio hiding ((%))
--import qualified Ratio
#if MAIN
import Ratio
#endif

-- powers = [[r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
powers :: [[Integer]]
powers = enumFrom 0 : map (zipWith (*) (head powers)) powers

-- powers = [[(-1)^r * r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
neg_powers :: [[Integer]]
neg_powers = 
  map (zipWith (\n x -> if n then x else negate x) (iterate not True)) powers

pascal:: [[Integer]]
pascal = iterate op [1,2,1]
op line = zipWith (+) (line++[0]) (0:line)


bernoulli :: Int -> Rational
bernoulli n = case n == 0 of
    True -> 1 % 1
    False -> case n == 1 of
        True -> 1 % 2
        False -> case odd n of
            True -> 0
            False -> let ps = neg_powers !! (subInt'2 n int1'0)
                     in (integer_1'0%integer2'0) `addRational'2` sumRational (zipWith (f ps) (enumFromTo int2'0 n) pascal)

f :: [Integer] -> Int -> [Integer] -> Rational
f powers k combs = ((sumInteger $ zipWith (mulInteger'2) powers (tail $ tail combs)) `subInteger'2` 
                            intToInteger'1 k) % intToInteger'1 (addInt'2 k int1'0)


root x = bernoulli x

#if MAIN
main = (\i -> root (i :: Int), 500 :: Int)
#endif

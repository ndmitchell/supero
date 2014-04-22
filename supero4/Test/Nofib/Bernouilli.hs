
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
import Data.Ratio
test = (\i -> root (i :: Int) :: Rational, 100 :: Int)

two = 2 :: Integer
zero = 0 :: Integer
zInt = 0 :: Int
rat1 :: Rational
rat1 = 0
#endif

#include "Include.hs"

-- powers = [[r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction

-- powers = [[(-1)^r * r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
neg_powers :: Int -> [Integer]
neg_powers n = 
    let powers = iterate (zipWith (*) [two..]) [two..]
    in map (zipWith (\n x -> if n then x else negate x) (iterate not True)) powers !! n

bernoulli :: Int -> Rational
bernoulli n =
    if n == zInt then rat1
    else if n == 1 then rat1
    else if odd n then rat1
    else let powers = neg_powers (n-1)
        in (-1)%2 
             + sum [ fromIntegral ((sum $ zipWith (*) powers (tail $ tail combs)) - 
                                    fromIntegral k) %
                     fromIntegral (k+1)
             | (k,combs)<- zip [2..n] $ iterate op [1,two,1]]

op line = zipWith (+) (line++[zero]) (zero:line)

root x = bernoulli x

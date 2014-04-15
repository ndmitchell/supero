
-- There was a lot of discussion about various ways of computing
-- Bernouilli numbers (whatever they are) on haskell-cafe in March 2003
-- Here's one of the programs.

-- It's not a very good test, I suspect, because it manipulates big integers,
-- and so probably spends most of its time in GMP.  

--import Prelude hiding ((!!),map,filter,odd,enumFromTo,error,zipWith,enumFrom,(++),iterate,($),tail,sum,not,head)
--import qualified Prelude
--import Ratio hiding ((%))
--import qualified Ratio
import Ratio

-- powers = [[r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
powers :: [[Integer]]
powers = enumFrom integer2'0 : map (zipWith (mulInteger'2) (head powers)) powers

-- powers = [[(-1)^r * r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
neg_powers :: [[Integer]]
neg_powers = 
  map (zipWith (\n x -> if n then x else negateInteger'1 x) (iterate not True)) powers

pascal:: [[Integer]]
pascal = iterate op (integer1'0:integer2'0:integer1'0:[])
op line = zipWith (addInteger'2) (line++[integer0'0]) (integer0'0:line)


bernoulli :: Int -> Rational
bernoulli n = case eqInt'2 n int0'0 of
    True -> integer1'0%integer1'0
    False -> case eqInt'2 n int1'0 of
        True -> integer_1'0%integer2'0
        False -> case odd n of
            True -> 0
            False -> let ps = neg_powers !! (subInt'2 n int1'0)
                     in (integer_1'0%integer2'0) `addRational'2` sumRational (zipWith (f ps) (enumFromTo int2'0 n) pascal)

f :: [Integer] -> Int -> [Integer] -> Rational
f powers k combs = ((sumInteger $ zipWith (mulInteger'2) powers (tail $ tail combs)) `subInteger'2` 
                            intToInteger'1 k) % intToInteger'1 (addInt'2 k int1'0)


root x = bernoulli x

#if IMPORT_SUPERO
import Ratio
#endif

#if MAIN_SUPERO
#endif

#if MAIN

intToInteger'1 = Prelude.fromIntegral :: Int -> Integer
eqInt'2 = (Prelude.==) :: Int -> Int -> Bool
mulInteger'2 = (Prelude.*) :: Integer -> Integer -> Integer
negateInteger'1 = Prelude.negate :: Integer -> Integer
addInteger'2 = (Prelude.+) :: Integer -> Integer -> Integer
subInt'2 = (Prelude.-) :: Int -> Int -> Int
addInt'2 = (Prelude.+) :: Int -> Int -> Int
modInt'2 = Prelude.mod :: Int -> Int -> Int
addRational'2 = (Prelude.+) :: Rational -> Rational -> Rational
gtInt'2 = (Prelude.>) :: Int -> Int -> Bool
subInteger'2 = (Prelude.-) :: Integer -> Integer -> Integer
rational'2 = (Ratio.%) :: Integer -> Integer -> Rational
error'1 = Prelude.error
sumRational = Prelude.sum :: [Rational] -> Rational
sumInteger = Prelude.sum :: [Integer] -> Integer

int0'0 = 0 :: Int
int1'0 = 1 :: Int
int2'0 = 2 :: Int
integer0'0 = 0 :: Integer
integer1'0 = 1 :: Integer
integer2'0 = 2 :: Integer
integer_1'0 = -1 :: Integer
rational0'0 = 0 :: Rational

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

enumFrom x = x : enumFrom (x `addInteger'2` integer1'0)

zipWith f x y = case x of
    [] -> []
    x:xs -> case y of
        [] -> []
        y:ys -> f x y : zipWith f xs ys

odd x = modInt'2 x 2 `eqInt'2` int1'0

sumInteger xs = case xs of
    [] -> integer0'0
    x:xs -> sumInteger2 x xs
sumInteger2 x xs = case xs of
    [] -> x
    y:ys -> sumInteger2 (y `addInteger'2` x) ys

sumRational xs = case xs of
    [] -> rational0'0
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
    x:xs -> case y `eqInt'2` int0'0 of
        True -> x
        False -> (!!) xs (y `subInt'2` int1'0)

enumFromTo from to = case from `gtInt'2` to of
    True -> []
    False -> from : enumFromTo (from `addInt'2` int1'0) to

#endif

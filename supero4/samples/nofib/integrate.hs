module Main (integrate1D, main) where

import System

integrate1D :: Double -> Double -> (Double->Double) -> Double
integrate1D l u f =
  let  d = (u-l)/8.0 in
     d * sum
        ((f l)*0.5 :
        f (l+d) :
        f (l+(2.0*d)) :
        f (l+(3.0*d)) :
        f (l+(4.0*d)) :
        f (u-(3.0*d)) :
        f (u-(2.0*d)) :
        f (u-d) :
        (f u)*0.5 : [])

integrate2D l1 u1 l2 u2 f = integrate1D l2 u2 
				    (\y->integrate1D l1 u1 
						  (\x->f x y))

zark u v = integrate2D 0.0 u 0.0 v (\x->(\y->x*y))

-- type signature required for compilers lacking the monomorphism restriction
ints = enumFrom 1.0 
zarks = zipWith zark ints (map (2.0*) ints)
rtotals = head zarks : zipWith (+) (tail zarks) rtotals

is = map (pow 4) ints
itotals = head is : zipWith (+) (tail is) itotals

es = map (pow 2) (zipWith (-) rtotals itotals)
etotal n = sum (take n es)

-- The (analytical) result should be zero
root n = etotal n

pow x y = y ^ x

#if MAIN
main = print $ root 5000

mul'2 = (*) :: Double -> Double -> Double
add'2 = (+) :: Double -> Double -> Double
sub'2 = (-) :: Double -> Double -> Double
div'2 = (/) :: Double -> Double -> Double
hat'2 = (^) :: Double -> Int -> Double
error'1 = error
eqIntZero'1 = (== 0) :: Int -> Bool
decInt'1 n = (n-1) :: Int -> Int

#endif

#if SUPERO
(*) = mul'2
(+) = add'2
(-) = sub'2
(/) = div'2
(^) = hat'2
error = error'1

enumFrom x = x : enumFrom (x+1.0)

zipWith f x y = case x of
    x:xs -> case y of
        y:ys -> f x y : zipWith f xs ys
        [] -> []
    [] -> []

head x = case x of
    [] -> error "head"
    x:xs -> x

tail x = case x of
    [] -> error "tail"
    x:xs -> xs

sum xs = case xs of
    [] -> 0
    x:xs -> sum2 x xs
sum2 x xs = case xs of
    [] -> x
    y:ys -> sum2 (y + x) ys

map f x = case x of
    y:ys -> f y : map f ys
    [] -> []

take n xs = case eqIntZero'1 n of
    True -> []
    False -> case xs of
        x:xs -> x : take (decInt'1 n) xs
        [] -> []
    


#endif


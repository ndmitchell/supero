
module Test.Other.Digits_of_e1_part2(test) where

#include "Include.hs"



aux n = 1:n:1:aux (jail n+2)


toDigits :: ContFrac -> [Integer]
toDigits xs = case xs of (x:xs) -> x:toDigits (jail (ratTrans (10,0,0,1) xs))

root n = take n $ toDigits $ 2:aux 2

#if MAIN
type ContFrac = [Integer]
-- ratTrans (a,b,c,d) x: compute (a + bx)/(c+dx) as a continued fraction 
ratTrans :: (Integer,Integer,Integer,Integer) -> ContFrac -> ContFrac
-- Output a digit if we can
ratTrans abcd xs = case abcd of
    (a,b,c,d) ->
        let q = b `div` d
        in case op'5 a b c d q of       -- Next digit is determined
                True -> q:ratTrans (c,d, (a-q*c), (b-q*d)) xs
                False -> case xs of
                    x:xs -> ratTrans (b, (a+x*b),d, (c+x*d)) xs

test = (root :: Int -> [Integer], 300 :: Int)

op'5 a b c d q = ((signum c == signum d) || (abs c < abs d)) && -- No pole in range
                 (c+d)*q <= a+b && (c+d)*q + (c+d) > a+b
#endif

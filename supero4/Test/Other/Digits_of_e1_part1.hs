
module Test.Other.Digits_of_e1_part1(test) where

#include "Include.hs"

type ContFrac = [Integer]


-- ratTrans (a,b,c,d) x: compute (a + bx)/(c+dx) as a continued fraction 
ratTrans :: (Integer,Integer,Integer,Integer) -> ContFrac -> ContFrac
-- Output a digit if we can
ratTrans abcd xs = case abcd of
    (aa,bb,cc,dd) ->
    	let a = jail aa in
    	let b = jail bb in
    	let c = jail cc in
    	let d = jail dd in

        let q = b `div` d
        in case op'5 a b c d q of       -- Next digit is determined
                True -> q:ratTrans (c,d, (a-q*c), (b-q*d)) xs
                False -> case xs of
                    x:xs -> ratTrans (b, (a+x*b),d, (c+x*d)) xs


root a b = ratTrans a b

#if MAIN
test = (\(a,n) -> take 100 $ root a (aux n), ((10,0,0,1), 20))

aux n = 1:n:1:aux (n+2)

op'5 a b c d q = ((signum c == signum d) || (abs c < abs d)) && -- No pole in range
                 (c+d)*q <= a+b && (c+d)*q + (c+d) > a+b
#endif

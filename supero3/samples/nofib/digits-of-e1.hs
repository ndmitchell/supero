

type ContFrac = [Integer]


eContFrac :: ContFrac
eContFrac = 2:aux 2
aux n = 1:n:1:aux (n+2)

-- ratTrans (a,b,c,d) x: compute (a + bx)/(c+dx) as a continued fraction 
ratTrans :: (Integer,Integer,Integer,Integer) -> ContFrac -> ContFrac
-- Output a digit if we can
ratTrans abcd xs = case abcd of
    (a,b,c,d) ->
        let q = b `div` d
        in case ((signum c == signum d) || (abs c < abs d)) && -- No pole in range
                 (c+d)*q <= a+b && (c+d)*q + (c+d) > a+b of       -- Next digit is determined
                True -> q:ratTrans (c,d,a-q*c,b-q*d) xs
                False -> case xs of
                    x:xs -> ratTrans (b,a+x*b,d,c+x*d) xs


toDigits :: ContFrac -> [Integer]
toDigits xs = case xs of (x:xs) -> x:toDigits (ratTrans (10,0,0,1) xs)

e :: [Integer]
e = toDigits eContFrac

root n = take n e

#if MAIN
main = print $ root 1000
#endif

#if SUPERO
(+) = addInteger'2
(==) = eqInteger'2
(<) = ltInteger'2
(<=) = leqInteger'2
abs = absInteger'1
(*) = mulInteger'2
(>) = gtInteger'2
(-) = subInteger'2
signum = signumInteger'1
div = divInteger'2

a || b = case a of
    True -> True
    False -> b

a && b = case a of
    True -> b
    False -> False

take n xs = case eqInt'2 n 0 of
    True -> []
    False -> case xs of
        x:xs -> x : take (subInt'2 n 1) xs
        [] -> []
    

#endif

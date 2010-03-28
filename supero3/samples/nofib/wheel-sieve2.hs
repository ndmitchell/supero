-- Mark II lazy wheel-sieve.
-- Colin Runciman (colin@cs.york.ac.uk); March 1996.
-- See article "Lazy wheel sieves and spirals of primes" (to appear, JFP).

import System

spiral ws ps qs = 
    case ws of
        [] -> error "spiral"
        w:ws -> case w of
            Wheel s ms ns -> 
                let sp = spiral ws (tail ps) (tail qs)
                    q = head qs
                in foldr (spiral_turn0 q sp) (spiral_roll s q sp ms ns s) ns

spiral_roll s q sp ms ns o = foldr (spiral_turn q sp o) (foldr (spiral_turn q sp o) (spiral_roll s q sp ms ns (o+s)) ns) ms

spiral_turn0  q sp n rs =
    if n<q then n:rs else sp
spiral_turn q sp o n rs =
    let n' = o+n in
    if n'==2 || n'<q then n':rs else dropWhile ((>=) n') sp


nextSize w p q = case w of
    (Wheel s ms ns) ->
        let xs_ns' = span ((>) q) (foldr (turn0 p) (roll p s ms ns (p-1) s) ns)
            xs = fst xs_ns'
            ns' = snd xs_ns'
            ms' = foldr (turn0 p) xs ms
        in Wheel (s*p) ms' ns'

roll p s ms ns t o = case t == 0 of
    True -> []
    False -> foldr (turn p o) (foldr (turn p o) (roll p s ms ns (t-1) (o+s)) ns) ms
turn0 p n rs =
    if n`mod`p>0 then n:rs else rs
turn p o n rs =
    let n' = o+n in
    if n'`mod`p>0 then n':rs else rs

#if MAIN
data Wheel = Wheel Int [Int] [Int]

main = print (primes !! (20000 :: Int) :: Int)

primes :: [Int]
primes = spiral wheels primes squares

squares :: [Int]
squares = [p*p | p <- primes]

wheels :: [Wheel]
wheels = Wheel 1 [1] [] :
         zipWith3 nextSize wheels primes squares 

intSub'2 = (-) :: Int -> Int -> Int
ltEq'2 = (<=) :: Int -> Int -> Bool
intAdd'2 = (+) :: Int -> Int -> Int
gt'2 = (>) :: Int -> Int -> Bool
gtEq'2 = (>=) :: Int -> Int -> Bool
mod'2 = mod :: Int -> Int -> Int
mul'2 = (*) :: Int -> Int -> Int
error'1 = error
primes'0 = primes

#endif

#if SUPERO
(-) = intSub'2
(<=) = ltEq'2
(+) = intAdd'2
(>) = gt'2
(>=) = gtEq'2
mod = mod'2
(*) = mul'2
error = error'1
primes = primes'0

fst x = case x of (a,b) -> a
snd x = case x of (a,b) -> b

foldr f z x = case x of
    []     -> z
    x:xs -> f x (foldr f z xs)

head x = case x of
    [] -> error "head"
    x:xs -> x
tail x = case x of
    [] -> error "tail"
    x:xs -> xs

span p xs = case xs of
    [] -> ([], [])
    (x:xs') -> case p x of
        True -> let res = span p xs' in (x:fst res,snd res)
        False -> ([], xs)
    
#endif


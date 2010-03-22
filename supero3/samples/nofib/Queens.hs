-- !!! count the number of solutions to the "n queens" problem.
-- (grabbed from LML dist)

module Main(main) where


#ifdef MAIN
main = print $ root 12
addInt'2 = (+) :: Int -> Int -> Int
subInt'2 = (-) :: Int -> Int -> Int
neqInt'2 = (/=) :: Int -> Int -> Bool
eqInt'2 = (==) :: Int -> Int -> Bool
gtInt'2 = (>) :: Int -> Int -> Bool

safer'3 x d q = x /= q && x /= q+d && x /= q-d 
#endif


root :: Int -> Int
root nq = length (gen nq nq)

safe :: Int -> Int -> [Int] -> Bool
safe x d q = case q of
    [] -> True
    q:l -> safer'3 x q d {- x /= q && x /= q+d && x /= q-d -} && safe x (d+1) l

gen :: Int -> Int -> [[Int]]
gen nq n = case n == 0 of
    True -> [[]]
    False -> 
        --[ (q:b) | b <- gen nq (n-1), q <- [1..nq], safe q 1 b]
        concatMap (\b -> concatMap (\q -> if safe q 1 b then [q:b] else []) (enumFromTo 1 nq)) (gen nq (n-1))


#if SUPERO

(+) = addInt'2
(-) = subInt'2
(/=) = neqInt'2
(>) = gtInt'2
(==) = eqInt'2


a && b = case a of
    True -> b
    False -> False

enumFromTo i j = if i > j then [] else i : enumFromTo (i+1) j

length x = case x of
    [] -> 0
    x:xs -> 1 + length xs

concatMap f x = case x of
    [] -> []
    x:xs -> f x ++ concatMap f xs

(++) xs ys = case xs of
    [] -> ys
    x:xs -> x : (xs ++ ys)

#endif

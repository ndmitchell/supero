
module Test where

{-
main :: Int -> Int -> Int
main n m = len [() | x <- list 1 n, y <- list 1 m]


len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

list :: Int -> Int -> [Int]
list i n = if i > n then []
                    else i : list (i+1) n
-}


-- !!! count the number of solutions to the "n queens" problem.
-- (grabbed from LML dist)

{-
main = do
    [arg] <- getArgs
    print $ nsoln $ read arg
-}

main nq = nsoln nq

nsoln nq = gen nq
    where
        safe :: Int -> Int -> [Int] -> Bool
        safe x d []    = True
        safe x d (q:l) = x /= q && safe x (d+1) l

        gen :: Int -> [[Int]]
        gen 0 = [[]]
        gen n = [ (q:b) | b <- gen (n-1), q <- [1..nq]]

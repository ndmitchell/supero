
module Test.Peter.Sumtree(test) where

sumtr t = case t of
    Leaf x -> x
    Branch l r -> sumtr l + sumtr r

squaretr t = case t of
    Leaf x -> Leaf (x*x)
    Branch l r -> Branch (squaretr l) (squaretr r)

buildTree n t = case n == 0 of
    True -> t
    False -> buildTree (n-1) (Branch t t)

root :: Int -> Int
root n = sumtr (squaretr (buildTree n (Leaf 1)))

#if MAIN

data Tree a = Leaf a | Branch (Tree a) (Tree a)

test = (\i -> root i :: Int, 22 :: Int)

eq'2 = (==) :: Int -> Int -> Bool
add'2 = (+) :: Int -> Int -> Int
sub'2 = (-) :: Int -> Int -> Int
mul'2 = (*) :: Int -> Int -> Int
#endif

#if SUPERO
(==) = eq'2
(+) = add'2
(*) = mul'2
(-) = sub'2
#endif


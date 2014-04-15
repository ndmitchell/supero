

x +& y = case x of
    Z -> y
    S x -> S (x +& y)

x *& y = case y of
    Z -> Z
    S y -> (x *& y) +& x


fromInteger_ x = if x < 1 then Z else S (fromInteger_ (x-1))

int :: Nat -> Int
int x = case x of
    Z     -> 0
    (S x) -> 1 + int x

x ^^^ y = case y of
    Z   -> S Z
    S y -> x *& (x ^^^ y)


root n = int (fromInteger_ 3 ^^^ fromInteger_ n)


#if MAIN

addInt'2 = (+) :: Int -> Int -> Int
subInt'2 = (-) :: Int -> Int -> Int
ltInt'2 = (<) :: Int -> Int -> Bool

data Nat = Z | S Nat

main = print $ (root (9::Int) :: Int)

#endif

#if SUPERO

(+) = addInt'2
(-) = subInt'2
(<) = ltInt'2

#endif

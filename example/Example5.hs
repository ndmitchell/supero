
module Example5 where

import Prelude hiding (head,fail,reverse,foldl)


data List a = Nil | Cons a (List a)

reverse = foldl (flop Cons) Nil

foldl f z Nil         = z
foldl f z (Cons x xs) = foldl f (f z x) xs

flop f x y = f y x

main x = map head (reverse x)

map f Nil = Nil
map f (Cons a b) = Cons (f a) (map f b)

fail = fail

head Nil = fail
head (Cons a b) = a


module Example1 where

import Prelude hiding (fail, head, map)

data List a = Nil | Cons a (List a)

fail = fail

head Nil = fail
head (Cons a b) = a

map f Nil = Nil
map f (Cons a b) = Cons (f a) (map f b)


main x = map head x

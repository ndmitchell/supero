
module Example3 where

import Prelude hiding (fail, head, map)

data List a = Nil | Cons a (List a)


main f x = map f (map head x)


map f Nil = Nil
map f (Cons a b) = Cons (f a) (map f b)


fail = fail

head Nil = fail
head (Cons a b) = a

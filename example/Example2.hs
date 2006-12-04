
module Example2 where

import Prelude hiding (fail, head, map)

data List a = Nil | Cons a (List a)


main f g x = map f (map g x)


map f Nil = Nil
map f (Cons a b) = Cons (f a) (map f b)

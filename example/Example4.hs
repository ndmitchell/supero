
module Example4 where

import Prelude hiding (even,odd,not,(.))

data List a = Nil | Cons a (List a)

odd x = div_by_2 x

div_by_2 x = div_by_2 x

even = not . odd

(.) f g x = f (g x)

not True = False
not False = True


main x = even x


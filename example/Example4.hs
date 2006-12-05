
module Example4 where

import Prelude hiding (even,odd)

even = not . odd

odd :: Int -> Bool
odd n = n `rem` 2 == 0

main x = even x


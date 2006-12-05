
module Example6 where

import Prelude hiding (head,fail,reverse,foldl)


data Expr = Add Expr Expr
          | Mul Expr Expr
          | Val Int


eval :: Expr -> Int
eval (Add x y) = eval x `plus` eval y
eval (Mul x y) = eval x `mul` eval y
eval (Val x) = x

plus x y = plus x y
mul x y = mul x y


main x y z = eval (Add (Mul (Val x) (Val y)) (Val z))


module Type(module Type, module Yhc.Core) where

import Yhc.Core
import Data.List


data CoreEx = CoreEx [CoreFuncEx]

data CoreFuncEx = CoreFuncEx String [CoreExpr] CoreExpr


instance Show CoreEx where
    show (CoreEx xs) = concat $ intersperse "\n\n" $ map show xs


instance Show CoreFuncEx where
    show (CoreFuncEx name args body) = name ++ concatMap ((' ':) . showCoreExprGroup) args ++ " = " ++ show body

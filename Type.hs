
module Type(module Type, module Yhc.Core) where

import Yhc.Core
import Data.List


data CoreEx = CoreEx [CoreFuncEx]

data CoreFuncEx = CoreFuncEx {coreFuncExName :: String
                             ,coreFuncExArgs :: [CoreExpr]
                             ,coreFuncExBody :: CoreExpr
                             }


instance Show CoreEx where
    show (CoreEx xs) = concat $ intersperse "\n\n" $ map show xs


instance Show CoreFuncEx where
    show (CoreFuncEx name args body) = name ++ concatMap ((' ':) . showCoreExprGroup) args ++ " = " ++ show body



drop1module :: Core -> Core
drop1module (Core name imports datas funcs) = Core name imports (map g datas) (concatMap h funcs)
    where
        f x = case break (== '.') x of
                   (_,"") -> x
                   (_,_:xs) -> xs
    
        g (CoreData name free args) = CoreData (f name) free (map g2 args)
        g2 (CoreCtor name items) = CoreCtor (f name) items
        
        h (CoreFunc name args body) 
            | name == "main" = []
            | otherwise = [CoreFunc (f name) args (mapUnderCore h2 body)]
        h2 (CoreFun x) = CoreFun $ f x
        h2 (CoreCon x) = CoreCon $ f x
        h2 x = x


isPrimitive (CorePos _ x) = isPrimitive x
isPrimitive (CoreApp x []) = isPrimitive x
isPrimitive (CoreVar x) = x == "primitive"
isPrimitive _ = False

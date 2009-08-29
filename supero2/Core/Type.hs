{-# LANGUAGE DeriveDataTypeable #-}

module Core.Type where

import Data.Generics


type VarName = String -- a locally defined variable
type FunName = String -- a top-level function
type ConName = String -- a constructor
type PrmName = String -- a primitive

type Pat = (ConName,[VarName])

data Core = Var VarName
          | Fun FunName
          | Con ConName
          | Prm PrmName 
          | App Core Core
          | Let VarName Core Core
          | Case Core [(Pat,Core)]
            deriving (Data,Typeable,Eq)

apps x (y:ys) = apps (App x y) ys
apps x [] = x

lets [] x = x
lets ((a,b):ys) x = Let a b $ lets ys x


isVar (Var _) = True; isVar _ = False
isCon (Con _) = True; isCon _ = False


fromApps (App x y) = (a,b ++ [y])
    where (a,b) = fromApps x
fromApps x = (x,[])

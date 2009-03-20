{-# LANGUAGE DeriveDataTypeable #-}

module Core.Type where

import Data.Generics

data Core = Var String -- a locally defined variable
          | Fun String -- a top-level function
          | Con String -- a constructor
          | Prim String -- a primitive
          | App Core Core
          | Lam String Core
          | Let String Core Core
          | Case Core [((String,[String]),Core)]
            deriving (Data,Typeable,Eq)


lams (x:xs) y = Lam x $ lams xs y
lams [] y = y

apps x (y:ys) = apps (App x y) ys
apps x [] = x

lets [] x = x
lets ((a,b):ys) x = Let a b $ lets ys x


isVar (Var _) = True; isVar _ = False
isCon (Con _) = True; isCon _ = False


fromApps (App x y) = (a,b ++ [y])
    where (a,b) = fromApps x
fromApps x = (x,[])

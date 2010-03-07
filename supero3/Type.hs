
module Type(
    Var, Fun, Con, Name(..), Exp(..), Env(..),
    env, fromHSE, toHSE
    ) where


import Language.Haskell.Exts hiding (Exp)
import Language.Haskell.Exts as HSE


type Var = String
type Fun = String
type Con = String

newtype Name = Name Fun Int Int

data Exp = Var  Name Var
         | Fun  Name Fun
         | Con  Name Con [Var]
         | Lam  Name Var Exp
         | Case Name Var [(Pat,Exp)
         | Let  Name [(Var,Exp)] Var

instance Show Exp where
    show = prettyPrint . toExp


data Env = Env {fun :: Fun -> Exp, prim :: Var -> Bool}



env :: [(Fun,Exp)] -> Env
env = undefined


---------------------------------------------------------------------
-- FROM HSE

fromHSE :: Module -> [(Fun,Exp)]
fromHSE = undefined


---------------------------------------------------------------------
-- TO HSE

toHSE :: [(Fun,Exp)] -> Module
toHSE = undefined


toExp :: Exp -> HSE.Exp
toExp = undefined

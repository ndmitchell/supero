{-# LANGUAGE DeriveDataTypeable #-}

module Core.Expr() where

import Core.Type
import Core.Show
import Core.Op
import Compiler.Expr
import Data.Generics
import Data.Generics.PlateData
import Data.List
import Control.Monad.State
import Control.Arrow

---------------------------------------------------------------------
-- SUPERCOMPILATION

instance Expr Core where
    (<<|) hist x = if null bad then Nothing else error $ show ("<|",x,head bad)
        where bad = filter (`homeo` x) hist

    step prog e'
        | dull e = Left $ residuate e
        | otherwise = Right [simplify $ gen $ resolve prog x | (Fun x,gen) <- contexts e]
        where e = simplify e'

    residual x xs = simplify $ apps x (map Fun xs)


suggest :: Core -> String
suggest (Fun x) = x
suggest (App x y) = suggest x
suggest (Lam x y) = suggest y
suggest _ = ""

---------------------------------------------------------------------
-- RESIDUATION

-- note: a lambda expresson on its own is not dull, as we'll very likely
-- need to clone all its free variables anyway!

dull :: Core -> Bool
dull (Lam _ x) = dull x
dull (Case (Var _) _) = True
dull (Con _) = True
dull (Var _) = True
dull (App x _) = f x
    where f (App x _) = f x
          f (Var _) = True
          f (Con _) = True
          f (Prim _) = True
          f _ = False
dull _ = False

residuate :: Core -> Residual Core
residuate x = (lams vars x2, map (suggest&&&id) es)
    where
        (vars,es) = unzip used
        (x2,(used,_)) = runState (f x) ([],fresh x)
    
        f :: Core -> State ([(String,Core)],[String]) Core
        f x | dull x = descendM f x
            | otherwise = do
                let vs = free x
                (used,w:ant) <- get
                put (used++[(w,lams vs x)],ant)
                return $ apps (Var w) (map Var vs)


---------------------------------------------------------------------
-- SIMPLIFICATION

simplify :: Core -> Core
simplify = transform f
    where
        f (App (Lam v x) y) = f $ Let v y x
        f (Let v x y) | cheap x || linear v y = simplify $ subst (v,x) y
        f (Case (Case on alts1) alts2) = simplify $ Case on [((a,b),Case c alts2) | ((a,b),c) <- alts1]
        f (Case on alts) | isCon ctr = simplify $ head [lets (zip b vs) c | ((a,b),c) <- alts, Con a == ctr]
            where (ctr,vs) = fromApps on
        f x = x

cheap (Fun _) = True
cheap (Var _) = True
cheap (Con _) = True
cheap _ = False


---------------------------------------------------------------------
-- HOMEOMORPHIC

data Shell = Shell String [Shell]


homeo :: Core -> Core -> Bool
homeo x y = hom (shell x) (shell y)

hom x y = dive x y || couple x y
dive x (Shell _ ys) = any (x `hom`) ys
couple (Shell x xs) (Shell y ys) = x == y && length xs == length ys && and (zipWith hom xs ys)


shell (Var _) = Shell "#Var" []
shell (Fun x) = Shell x []
shell (Con x) = Shell x []
shell (Prim x) = Shell x []
shell (App x y) = Shell "#App" [shell x,shell y]
shell (Lam _ x) = Shell "#Lam" [shell x]
shell (Let _ x y) = Shell "#Let" [shell x,shell y]
shell (Case x ys) = Shell "#Case" $ shell x : map (shell . snd) ys

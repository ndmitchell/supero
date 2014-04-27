{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Simplify(simplifys, simplify, etas) where

import Util hiding (fresh)
import Exp
import Control.Arrow
import Data.Generics.Uniplate.Data


etas :: [(Var,Exp)] -> [(Var,Exp)]
etas env = map (second $ transformBi f) env
    where
        f (Var x) | Just (fromLams -> (vs, _)) <- lookup x env = lams vs $ apps (Var x) $ map Var vs
        f x = x


simplifys :: [(Var,Exp)] -> [(Var,Exp)]
simplifys = map (second simplify)

simplify :: Exp -> Exp
simplify = \(relabel -> x) -> equivalent "simplify" x $ idempotent "simplify" fs x
    where
        fs = transform f

        f o@(App (fromLets -> (bs@(_:_), Lam v z)) q) = fs $ Let v q $ lets bs z
        f o@(Case (Let v x y) alts) = fs $ Let v x $ Case y alts
        {-
        -- True, but a bit different to the others, since it is information propagation
        -- Nothing requries it yet
        f o@(Case (Var v) alts) | map g alts /= alts = fs $ Case (Var v) $ map g alts
            where g (PCon c vs, x) | v `notElem` vs = (PCon c vs, subst [(v, apps (Con c) $ map Var vs)] x)
                  g x = x
        -}
        f (App (Lam v x) y) = f $ Let v y x
        f (Let v x y) | cheap x || linear v y = fs $ subst [(v,x)] y
        f o@(Case (Case on alts1) alts2) =  fs $ Case on $ map g alts1
            where g (PWild, c) = (PWild, Case c alts2)
                  g (PCon a vs, c) = (PCon a vs, Case c alts2)
        f x | Just ((unzip -> (vs, xs)), bod) <- caseCon x = fs $ lets (zip vs xs) bod
        f x = x

cheap (Var _) = True
cheap (Con _) = True
cheap (Lam _ _) = True
cheap _ = False


linear :: Var -> Exp -> Bool
linear v x = count v x <= 1

count :: Var -> Exp -> Int
count v (Var x) = if v == x then 1 else 0
count v (Lam w y) = if v == w then 0 else count v y * 2 -- lambda count is infinite, but 2 is close enough
count v (Let w x y) = count v x + (if v == w then 0 else count v y)
count v (Case x alts) = count v x + maximum [if v `elem` varsP p then 0 else count v c | (p,c) <- alts]
count v (App x y) = count v x + count v y
count v _ = 0

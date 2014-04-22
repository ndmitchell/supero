{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Simplify(simplifys, simplify) where

import Util hiding (fresh)
import Exp
import Control.Applicative
import Control.Arrow
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Generics.Uniplate.Data
import Debug.Trace


simplifys :: [(Var,Exp)] -> [(Var,Exp)]
simplifys = map (second simplify)

simplify :: Exp -> Exp
simplify = \(relabel -> x) -> equivalent "simplify" x $ idempotent "simplify" fs x
    where
        fs = transform f

        f o@(App (fromLets -> (bs@(_:_), Lam v z)) q) = fs $ Let v2 q $ lets bs $ subst [(v,Var v2)] z
            where v2:_ = fresh $ vars o
        f o@(Case (Let v x y) alts) = fs $ Let v2 x $ Case (subst [(v,Var v2)] y) alts
            where v2:_ = fresh $ vars o
        {-
        -- True, but a bit different to the others, since it is information propagation
        -- Nothing requries it yet
        f o@(Case (Var v) alts) | map g alts /= alts = fs $ Case (Var v) $ map g alts
            where g (PCon c vs, x) | v `notElem` vs = (PCon c vs, subst [(v, apps (Con c) $ map Var vs)] x)
                  g x = x
        -}
        f (App (Lam v x) y) = f $ Let v y x
        f (Let v x y) | cheap x || linear v y = fs $ subst [(v,x)] y
        f o@(Case (Case on alts1) alts2) = fs $ Case on $ map g alts1
            where new = fresh $ vars o
                  g (PWild, c) = (PWild, Case c alts2)
                  g (PCon a vs, c) = let vs2 = take (length vs) new
                                     in (PCon a vs2, Case (subst (zip vs $ map Var vs2) c) alts2)
        f x | Just ((unzip -> (vs, xs)), bod) <- caseCon x =
            let vs2 = take (length vs) $ fresh $ vars x
            in fs $ lets (zip vs2 xs) $ subst (zip vs $ map Var vs2) bod
        f x = x

cheap (Var _) = True
cheap (Con _) = True
cheap (Lam _ _) = True
cheap _ = False

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
simplify = \x -> let y = transform f x; yy = transform f y in if y == yy then y else
                 error $ "Simplify not idempotent!\nOriginal\n" ++ pretty x ++ "\nFirst result\n" ++ pretty y ++ "\nSecond result\n" ++ pretty yy
    where
        fs = transform f

        f o@(App (fromLets -> (bs@(_:_), Lam v z)) q) = fs $ Let v2 q $ lets bs $ subst [(v,Var v2)] z
            where v2:_ = fresh $ vars o
        f o@(Case (Let v x y) alts) = fs $ Let v2 x $ Case (subst [(v,Var v2)] y) alts
            where v2:_ = fresh $ vars o
        f (App (Lam v x) y) = f $ Let v y x
        f (Let v x y) | cheap x || linear v y = fs $ subst [(v,x)] y
        f (Case (Case on alts1) alts2) = fs $ Case on [(a,Case c alts2) | (a,c) <- alts1]
        f o@(Case (fromApps -> (Con ctr, xs)) alts) = headNote ("Couldn't match constructor: " ++ pretty o) $ mapMaybe g alts
            where g (PWild, x) = Just $ x
                  g (PCon c vs, x) | c == ctr = Just $ fs $ lets (zip vs xs) x
                                   | otherwise = Nothing
        f x = x

cheap (Var _) = True
cheap (Con _) = True
cheap (Lam _ _) = True
cheap _ = False

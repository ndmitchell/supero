{-# LANGUAGE DeriveDataTypeable #-}

module Core.Op where

import Core.Type
import Data.List
import Data.Generics.PlateData


variables :: Core -> [String]
variables = concatMap f . universe
    where
        f (Var x) = [x]
        f (Lam x _) = [x]
        f (Case _ xs) = concatMap (snd . fst) xs
        f _ = []


free :: Core -> [String]
free (Var x) = [x]
free (Lam x y) = free y \\ [x]
free (Case _ xs) = nub $ concat [free c \\ b | ((a,b),c) <- xs]
free (Let v x y) = nub $ free x ++ (free y \\ [v])
free x = nub $ concatMap free $ children x

fresh :: Core -> [String]
fresh x = ["x" ++ show i | i <- [1..]] \\ variables x 

-- perform hygenic substitution
subst :: (String,Core) -> Core -> Core
subst (v,x) (Var q) | q == v = x
subst (v,x) (Lam w y) | v == w = Lam w y
subst (v,x) (Let w y z) | v == w = Let w y z
subst (v,x) (Case y as) = Case (subst (v,x) y) [((a,b), if v `elem` b then c else subst (v,x) c) | ((a,b),c) <- as]
subst (v,x) y = descend (subst (v,x)) y


linear :: String -> Core -> Bool
linear v x = count v x <= 1

count :: String -> Core -> Int
count v (Var x) = if v == x then 1 else 0
count v (Lam w y) = if v == w then 0 else count v y
count v (Let w x y) = if v == w then 0 else count v x + count v y
count v (Case x alts) = count v x + maximum [if v `elem` b then 0 else count v c | ((a,b),c) <- alts]
count v x = sum $ map (count v) $ children x

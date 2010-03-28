
module Simpler(simpler) where

import Data.List
import Data.Maybe
import Type
import Util

simpler = removeDeadArgs



removeDeadArgs :: [(Var,Exp)] -> [(Var,Exp)]
removeDeadArgs xs = error $ show dead
    where
        dead = fixEq (deadArgs xs) [(v,i) | (v,x) <- xs, let FlatExp free _ _ = toFlat x, i <- [0..length free-1]]



deadArgs :: [(Var,Exp)] -> [(Var,Int)] -> [(Var,Int)]
deadArgs xs dead = filter f dead
    where
        f (v,i) = v /= root && all (g (free!!i) . snd) bind
            where FlatExp free bind root = toFlat $ fromJust $ lookup v xs
        
        g v (App _ op xs) | op /= v = all (`elem` dead) $ map ((,) op) $ findIndices (== v) xs
        g v x = v `notElem` free x

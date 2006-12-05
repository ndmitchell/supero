
module Normalise where

import Type
import Match

import qualified Data.Map as Map
import Data.Play
import Data.List
import Data.Maybe
import General


normalise :: Prog -> Prog
normalise prog = prog{funcs = Map.map normaliseFunc (funcs prog)}


normaliseFunc :: Func -> Func
normaliseFunc (Func name body) = Func name [alt{altBody = simplify (altBody alt)} | alt <- body]



populate :: Prog -> Prog
populate (Prog funcs) = Prog $ Map.map insert funcs
    where
        news = collect funcs
        
        insert func = func{funcAlts = newalts2 ++ funcAlts func}
            where
                oldalt = altNum $ head $ funcAlts func
                newalts2 = reverse $ zipWith (\n x -> x{altNum=n}) [oldalt+1..] $ reverse newalts
                newalts = [FuncAlt 0 args (simplify $ inlineExpr funcs call args)
                          | Apply (Fun call) args <- news, call == funcName func]



collect :: FuncMap -> [Expr]
collect funcs = nub $ filter isValid $ concatMap (allOver . altBody) $ concatMap funcAlts $ Map.elems funcs
    where
        isValid (Apply (Fun call) args)
            = (isNothing $ findExactRhs func args) && (isJust $ findBestRhs func args)
            where func = fromJust $ Map.lookup call funcs
        isValid _ = False


inline :: Prog -> Prog
inline (Prog funcs) = Prog $ Map.map f funcs
    where
        f func = func{funcAlts = map (g 5) (funcAlts func)}

        g 0 (FuncAlt i lhs rhs) = FuncAlt i lhs (simplify rhs)

        g n (FuncAlt i lhs (Case (Apply (Fun call) args) alts)) =
            g (n-1) (FuncAlt i lhs (Case (inlineExpr funcs call args) alts))
        g _ x = g 0 x



inlineExpr :: FuncMap -> String -> [Expr] -> Expr
inlineExpr funcs call args =
    case findBestRhs (fromJust $ Map.lookup call funcs) args of
        Nothing -> Apply (Fun call) args
        Just x -> thd3 x


simplify :: Expr -> Expr
simplify = mapUnder f
    where
        f (Apply (Apply x xs) ys) = f $ Apply x (xs++ys)
    
        f (Case (Case on alts1) alts2) = f $ Case on (map g alts1)
            where g (lhs,rhs) = (lhs, f $ Case rhs alts2)

        f (Case on@(Apply (Ctr x) xs) alts) = replaceBinding bind rhs
            where
                (bind,rhs) = head [(bind,rhs) | (lhs,rhs) <- alts, Just bind <- [matchBinding [lhs] [on]]]

        f x = x



module Normalise where

import Type
import qualified Data.Map as Map
import Data.Play
import Data.List
import Data.Maybe


normalise :: Prog -> Prog
normalise prog = prog{funcs = Map.map normaliseFunc (funcs prog)}


normaliseFunc :: Func -> Func
normaliseFunc (Func name body) = Func name [(a,simplify b) | (a,b) <- body]



populate :: Prog -> Prog
populate (Prog funcs) = Prog $ Map.map insert funcs
    where
        news = collect funcs
        
        insert func = func{funcAlts = newalts ++ funcAlts func}
            where
                newalts = [(args,simplify $ inlineExpr funcs call args)
                          | Apply (Fun call) args <- news, call == funcName func]



collect :: FuncMap -> [Expr]
collect funcs = nub $ filter isValid $ concatMap (allOver . snd) $ concatMap funcAlts $ Map.elems funcs
    where
        isValid (Apply (Fun call) args)
            | not (any isJail args)
            = (isNothing $ findExactRhs func args) && (isJust $ findBestRhs func args)
            where func = fromJust $ Map.lookup call funcs
        isValid _ = False


inline :: Prog -> Prog
inline (Prog funcs) = Prog $ Map.map f funcs
    where
        f func = func{funcAlts = map (g 5) (funcAlts func)}

        g 0 (lhs,rhs) = (lhs,simplify rhs)        

        g n (lhs,Case (Apply (Fun call) args) alts) = g (n-1) (lhs, Case (inlineExpr funcs call args) alts)
        g _ x = g 0 x



inlineExpr :: FuncMap -> String -> [Expr] -> Expr
inlineExpr funcs call args =
    case findBestRhs (fromJust $ Map.lookup call funcs) args of
        Nothing -> Apply (Fun call) args
        Just x -> x


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


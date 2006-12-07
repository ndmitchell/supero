
module Normalise where

{-
There are two types of "knowledge loosing" operations in this language:

[CALL_EVAL] Apply (Fun x) (... (Eval ...) ....)
[CASE_CALL] Case (Apply (Fun x) ...)

The aim is to eliminate both types.

The first is eliminated by collecting specialised versions that are being asked for.

The second is eliminated by inlining.
-}

import Type
import Match

import qualified Data.Map as Map
import Data.Play
import Data.List
import Data.Maybe
import General


---------------------------------------------------------------------
-- CALL EVAL
--
-- Detect where calls are being made with an embedded eval
-- generate new versions based on this.


call_eval :: Prog -> Prog
call_eval (Prog funcs) = Prog $ Map.map insert funcs
    where
        news = collect funcs
        
        insert func = func{funcAlts = newalts2 ++ funcAlts func}
            where
                oldalt = altNum $ head $ funcAlts func
                newalts2 = reverse $ zipWith (\n x -> x{altNum=n}) [oldalt+1..] $ reverse newalts
                newalts = [FuncAlt 0 args (simplifyExpr $ inlineExpr funcs call args)
                          | Apply (Fun call n) args <- news, call == funcName func]


-- | Put together a list of function calls which need special instances generating
--   Need to do this if:
--   * The best one is not that close
--   * Will not result in it calling itself
collect :: FuncMap -> [Expr]
collect funcs = nub $ concat [f func alt expr
              | func <- Map.elems funcs, alt <- funcAlts func, expr <- allOver (altBody alt)]
    where
        f func alt (Apply (Fun call n) args)
            | (isNothing $ findExactRhs func args) &&
              (isJust unfold) && noSelf
            = [Apply (Fun call n) args2]
            where
                args2 = args -- should be blurring here! map (blur 3) args
            
                func = fromJust $ Map.lookup call funcs
                unfold = findBestRhs func args
                
                noSelf = not $ any isSelf $ allOver $ thd3 $ fromJust unfold
                isSelf (Apply (Fun call n) args) =
                        call == funcName func && isJust res &&
                        fst3 (fromJust res) == altNum alt &&
                        length args == length (altMatch alt)
                    where res = findBestRhs func args
                isSelf _ = False

        f _ _ _ = []



---------------------------------------------------------------------
-- CASE CALL
--
-- Detect where calls are being made inside a case.
-- Do inline expansion if possible.


case_call :: Prog -> Prog
case_call (Prog funcs) = Prog $ Map.map f funcs
    where
        f func = func{funcAlts = map (g 5) (funcAlts func)}

        g 0 (FuncAlt i lhs rhs) = FuncAlt i lhs (simplifyExpr rhs)

        g n (FuncAlt i lhs (Case (Apply (Fun call _) args) alts)) =
            g (n-1) (FuncAlt i lhs (Case (inlineExpr funcs call args) alts))
        g _ x = g 0 x







---------------------------------------------------------------------
-- SIMPLIFICATION
--
-- Take a program and make it simpler by applying localised rules
-- such as case (case ..) etc


simplify :: Prog -> Prog
simplify (Prog funcs) = Prog $ Map.map simplifyFunc funcs


simplifyFunc :: Func -> Func
simplifyFunc (Func name body) = Func name [alt{altBody = simplifyExpr (altBody alt)} | alt <- body]


simplifyExpr :: Expr -> Expr
simplifyExpr = mapUnder f
    where
        f (Apply (Apply x xs) ys) = f $ Apply x (xs++ys)
    
        f (Case (Case on alts1) alts2) = f $ Case on (map g alts1)
            where g (lhs,rhs) = (lhs, f $ Case rhs alts2)

        f (Case on@(Apply (Ctr x) xs) alts) = replaceBinding bind rhs
            where
                (bind,rhs) = head [(bind,rhs) | (lhs,rhs) <- alts, Just bind <- [matchBinding [lhs] [on]]]

        f x = x



---------------------------------------------------------------------
-- UTILITIES


blur :: Int -> Expr -> Expr
blur 0 _ = Var 0 -- needs to be a unique variable!
blur n x = generate (map (blur (n-1)) children)
    where (children,generate) = replaceChildren x



inlineExpr :: FuncMap -> String -> [Expr] -> Expr
inlineExpr funcs call args =
    case findBestRhs (fromJust $ Map.lookup call funcs) args of
        Nothing -> Apply (Fun call 0) args
        Just x -> thd3 x


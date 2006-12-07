
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
import Convert
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


-- the list of data structures than cannot be specialised
type Call_Eval = Map.Map String [Int]


call_eval_analysis :: Prog -> Call_Eval
call_eval_analysis prog = Map.fromList [("foldl",[1])]


call_eval :: Call_Eval -> Prog -> Prog
call_eval analysis (Prog funcs) = Prog $ removeEval $ create reqs funcs2
    where
        funcs2 = insertEval funcs
        reqs = required analysis funcs2


-- use the calls that were just created
use :: FuncMap -> FuncMap
use funcs = Map.map f funcs
    where
        f func = func{funcAlts = map (\alt -> alt{altBody = mapOver g (altBody alt)}) (funcAlts func)}
        
        g (Apply (FunAlt x i) xs) | any isEval xs && isJust rhs = Apply (FunAlt x i2) xs2
            where
                xs2 = map (mapUnder fromEval) xs
                rhs = findExactRhs (fromJust $ Map.lookup x funcs) xs2
                Just (i2,_,_) = rhs

        g x = x


-- create all the required calls
create :: [Expr] -> FuncMap -> FuncMap
create req funcs = Map.map f funcs
    where
        f func = func{funcAlts = newalts2 ++ funcAlts func}
            where
                oldalt = altNum $ head $ funcAlts func
                newalts2 = reverse $ zipWith (\n x -> x{altNum=n}) [oldalt+1..] $ reverse newalts
                newalts = [FuncAlt 0 args (simplifyExpr $ inlineExpr funcs call (map (mapUnder toFunAlt) args))
                          | Apply (Fun call) args <- req, call == funcName func]


-- figure out which calls are required
required :: Call_Eval -> FuncMap -> [Expr]
required analysis funcs = nub $ concat [f func alt expr
               | func <- Map.elems funcs, alt <- funcAlts func, expr <- allOver (altBody alt)]
    where
        f func alt (Apply (FunAlt call n) args)
            | any isEval args2 = [Apply (Fun call) (map removeEvalExpr args2)]
            where
                args2 = zipWith3 g [0..] args free
                free = [1..] \\ [i | Var i <- args]
                res = Map.findWithDefault [] call analysis
                
                g i arg fre | i `elem` res = Var fre
                            | otherwise = arg
            
        f _ _ _ = []


-- make all Fun -> FunAlt
-- insert Eval where required
insertEval :: FuncMap -> FuncMap
insertEval funcs = Map.map (onBody_Func f) funcs
    where
        f (Apply (Fun x) args) | isJust res = Apply (FunAlt x n) (map (replaceBinding bind2) alt ++ extraArgs)
            where
                extraArgs = map Eval $ drop (length alt) args
            
                func = fromJust $ Map.lookup x funcs

                res = findBestRhs func args
                Just (n,bind,rep) = res
                alt = altMatch $ getFuncAlt func n
                
                vars = [i | Var i <- map snd bind]
                goodvar = nub vars \\ (vars \\ nub vars)
                bind2 = [(a, g b) | (a,b) <- bind]
                
                g (Var i) | i `elem` goodvar = Var i
                g x = Eval $ f x

        f x = generate (map f children)
            where (children,generate) = replaceChildren x


removeEval :: FuncMap -> FuncMap
removeEval = onBody_Funcs removeEvalExpr


removeEvalExpr :: Expr -> Expr
removeEvalExpr = mapUnder f
    where
        f (FunAlt x i) = Fun x
        f (Eval x) = x
        f x = x

---------------------------------------------------------------------
-- CASE CALL
--
-- Detect where calls are being made inside a case.
-- Do inline expansion if possible.


case_call :: Prog -> Prog
case_call (Prog funcs) = simplify $ Prog $ Map.map f funcs
    where
        f func = func{funcAlts = map g (funcAlts func)}
        g (FuncAlt i lhs rhs) = FuncAlt i lhs (mapUnder h rhs)
        
        h (Case (Apply (Fun call) args) alts) =
            (Case (inlineExpr funcs call args) alts)
        h x = x



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
        Nothing -> Apply (Fun call) args
        Just x -> thd3 x

        

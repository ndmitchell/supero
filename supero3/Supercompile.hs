{-# LANGUAGE PatternGuards #-}

module Supercompile(supercompile) where

import Type
import Simplify
import Terminate
import Util
import Data.List
import Data.Maybe
import Data.Generics.Uniplate.Data hiding (children)

---------------------------------------------------------------------
-- MANAGER

data Tree = Tree {pre :: Exp, gen :: [Var] -> Exp, children :: [Tree]}


supercompile :: Env -> [(Var,Exp)]
supercompile env = assign $ flatten $ optimise env $ fromJust $ env "main"


optimise :: Env -> Exp -> Tree
optimise env = f newHistory
    where  f t x | terminate (<=|) t x = g x (stop t x) t
                 | otherwise = g x (reduce env x) (x += t)
           g x (gen,cs) t = Tree x gen (map (f t) cs)


reduce :: Env -> Exp -> ([Var] -> Exp, [Exp])
reduce env = f newHistory
    where f t x | terminate (<|) t x = stop t x
                | Just x' <- step env x = f (x += t) x'
                | otherwise = split x


flatten :: Tree -> [Tree]
flatten = nubBy (\x y -> pre x == pre y) . f []
    where f seen t  =  if pre t `elem` seen then [] else
                       t : concatMap (f (pre t:seen)) (children t)


assign :: [Tree] -> [(Var,Exp)]
assign ts = [(f t, gen t (map f (children t))) |  t <- ts]
    where f t = fromJust $ lookup (pre t) names
          names = zip (map pre ts) freshNames 


freshNames = ["f" ++ show i | i <- [1..]]


---------------------------------------------------------------------
-- BOX NOTATION AND STACKS

-- find the variable bound at the top of the stack
stackTop :: FlatExp -> Maybe (Var,Exp)
stackTop (FlatExp _ bind v) = f Nothing v
    where f res v = case lookup v bind of
                         Nothing -> res
                         Just e | Just w <- fmap fst $ force e -> f (Just (v,e)) w
                                | otherwise -> Just (v,e)


-- ensure the top of the stack is a variable bound to a variable
-- i.e. stack1 = v (where v does not have a binding)
-- if at all possible (can't if there is a constructor at the top for example)
stackVar :: FlatExp -> FlatExp
stackVar flat@(FlatExp free bind root) = case stackTop flat of
    Nothing -> FlatExp free [("_fake",Var noname root)] "_fake"
    Just (v,e) | Just (v2,c2) <- force e -> FlatExp free (("_fake",Var noname v2):(v,c2 "_fake"):delFst v bind) root
    _ -> flat


force :: Exp -> Maybe (Var, Var -> Exp)
force (Var n x) = Just (x, Var n)
force (App n x y) = Just (x, \x -> App n x y)
force (Case n x y) = Just (x, \x -> Case n x y)
force (Let n x y) = Just (y, Let n x)
force _ = Nothing


debox :: Exp -> ([Var] -> Exp, [Exp])
debox = deboxName . deboxFree


-- name and extract the Box components
deboxName :: Exp -> ([Var] -> Exp, [Exp])
deboxName x = error $ "debox name\n:" ++ pretty x


-- simplify and give sufficient free variables to Box bits
deboxFree :: Exp -> Exp
deboxFree o = transform f o
    where
        fo = free o

        f (Box x) = apps (Box $ simplify $ lams vs x2) vs
            where
                vs = sort $ fx2 \\ fo
                x2 = simplify x
                fx2 = free x2
        f x = x

        apps :: Exp -> [Var] -> Exp
        apps x [] = x
        apps x xs = Let noname (zip vs $ x : zipWith (App noname) vs (reverse xs)) (last vs)
            where vs = ["_box" ++ show i | i <- [0..length xs]]


---------------------------------------------------------------------
-- OPERATIONS

step :: Env -> Exp -> Maybe Exp
step env x | Just (v,Var _ f) <- stackTop flat, Just e <- env f =
    Just $ simplify $ fromFlat $ FlatExp free ((v,e):delFst v bind) root
    where flat@(FlatExp free bind root) = stackVar $ toFlat x
step env x = Nothing


split :: Exp -> ([Var] -> Exp, [Exp])
split x
    | Just (_, Case n v xs) <- s = 
        let alt (p,x) = (p, Box $ Let noname ((v,p):bind) root)
        in debox $ lams free $ Case noname v $ map alt xs
    | otherwise =
        error $ pretty x ++ "\n" ++ show s
    where
        s = stackTop flat
        flat@(FlatExp free bind root) = toFlat x


stop :: History -> Exp -> ([Var] -> Exp, [Exp])
stop _ x = error $ "stop: " ++ pretty x


{-
\free ->  let  s_1  = case x of p_1 -> e_1' ; p_m -> e_m'
               v_1  = e_1
               v_n  = e_n
          in   v
\end{code}

\noindent becomes:

\begin{code}
\free -> case x of
    p_1  -> <? let  s_1 = case x of p_1 -> e_1'; p_m -> e_m'
                    v_1 = e_1; v_n = e_n; x = p_1 in v ?>
    p_m  -> <? let  s_1 = case x of p_1 -> e_1'; p_m -> e_m'
                    v_1 = e_1; v_n = e_n; x = p_m in v ?>
-}

{-# LANGUAGE PatternGuards #-}

module Supercompile(supercompile) where

import Type
import Terminate
import Data.List
import Data.Maybe

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


---------------------------------------------------------------------
-- OPERATIONS

step :: Env -> Exp -> Maybe Exp
step env x = error $ "step: " ++ pretty x


split :: Exp -> ([Var] -> Exp, [Exp])
split x = error $ "split: " ++ pretty x


stop :: History -> Exp -> ([Var] -> Exp, [Exp])
stop _ x = error $ "stop: " ++ pretty x

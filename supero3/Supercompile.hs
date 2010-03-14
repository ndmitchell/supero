{-# LANGUAGE PatternGuards #-}

module Supercompile(supercompile) where

import Type
import Simplify
import Terminate
import Util
import Data.List
import Data.Maybe
import Control.Arrow
import Data.Generics.Uniplate.Data hiding (children)

---------------------------------------------------------------------
-- MANAGER

data Tree = Tree {pre :: Exp, gen :: [Var] -> Exp, children :: [Tree]}


supercompile :: Env -> [(Var,Exp)]
supercompile env = resetTime $ assign $ flatten $ optimise env $ fromJust $ env "main"


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
assign ts = -- error $ pretty $ ($ repeat "?") $ fst $ split $
       -- fromJust $ lookup "f12" [(b,a) | (a,b) <- names]
        [(f t, gen t (map f (children t))) |  t <- ts]
    where f t = fromJust $ lookup (pre t) names
          names = zip (map pre ts) freshNames 


freshNames = ["f" ++ show i | i <- [1..]]


---------------------------------------------------------------------
-- STACKS

-- find the variable bound at the top of the stack
-- only returns Nothing if no bound variables
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


---------------------------------------------------------------------
-- BOXES

debox :: Exp -> ([Var] -> Exp, [Exp])
debox = deboxName . deboxFree


-- name and extract the Box components
deboxName :: Exp -> ([Var] -> Exp, [Exp])
deboxName x = (regen, boxes)
    where
        boxes = nub [y | Box y <- universe x]
        regen names = transform f x
            where vs = zip boxes names
                  f (Box x) = Var noname $ fromJust $ lookup x vs
                  f x = x


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
        apps x xs = Let noname (zip vs $ x : zipWith (App noname) vs xs) (last vs)
            where vs = ["_box" ++ show i | i <- [0..length xs]]


---------------------------------------------------------------------
-- SHARING

simplifyBox = transform f
    where f (Box x) = Box $ simplify x
          f x = x


-- given a set of expressions
-- bound to boxes, you may move an expression under a box if it's only used by one
share :: (Exp -> Bool) -> Exp -> Exp
share test = f . simplifyBox
    where f x = head $ [f x2 | (v,x2) <- promote "v11" $ shareOptions x, test x2] ++ [x]
    
          promote x ys = bs ++ as
            where (as,bs) = partition ((==) x . fst) ys


-- each variable is bound at the let, to a box
-- is used in at most one binding, and not the root
-- and the binding it is used at is a box
shareOptions :: Exp -> [(Var, Exp)]
shareOptions x =
        [ (v, fromFlat $ FlatExp vars (map inject used ++ delFsts (v:used) bind) root)
        | (v, Box vx) <- bind, v `notElem` bad, let used = [w | (w,e) <- bind, v `elem` free e], length used <= 1 || cheap vx
        , let inject w = (w,Box $ simplify $ Let noname [(v,vx),("_share",fromBox $ fromJust $ lookup w bind)] "_share")
        ]
    where
        fromBox (Box x) = x
        bad = nub $ root : concat [free e | (_,e) <- bind, not $ isBox e]
        frees = concatMap (free . snd) bind

        FlatExp vars bind root = toFlat x

cheap Var{} = True
cheap Con{} = True
cheap _ = False

---------------------------------------------------------------------
-- OPERATIONS

step :: Env -> Exp -> Maybe Exp
step env x | Just (v,Var _ f) <- stackTop flat, Just e <- env f =
    Just $ simplify $ fromFlat $ FlatExp free ((v,e):delFst v bind) root
    where flat@(FlatExp free bind root) = stackVar $ toFlat x
step env x = Nothing


split :: Exp -> ([Var] -> Exp, [Exp])
split x
    | Nothing <- s = (const x, [])
    | Just (_, Case n v xs) <- s = 
        let alt (p,x) = (p, Box $ Let noname ((v,p):bind) root)
        in debox $ lams free $ Case noname v $ map alt xs
    | Just (v, Lam{}) <- s =
        debox $ share (const True) $ fromFlat $ FlatExp free [(a, if a == v then boxlam b else Box b) | (a,b) <- bind] root
    | Just (v, _) <- s =
        debox $ share (const True) $ fromFlat $ FlatExp free [(a, if a == v then b else Box b) | (a,b) <- bind] root
    where
        s = stackTop flat
        flat@(FlatExp free bind root) = toFlat x
        boxlam (Lam n v x) = Lam n v $ boxlam x
        boxlam x = Box x


stop :: History -> Exp -> ([Var] -> Exp, [Exp])
stop hist x = if time 2 then 
        error $ "STOP:\n" ++ prettyNames x ++ "\n ==>\n" ++ prettyNames res
        else debox res
    where
        res = share (all f . universe) $ fromFlat $ FlatExp free (map (second Box) bind) root
        FlatExp free bind root = toFlat x

        f (Box x) = not $ terminate (<=|) hist x
        f _ = True

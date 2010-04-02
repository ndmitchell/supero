{-# LANGUAGE PatternGuards #-}

{-
TODO:
cheaps are merged multiple times, should do sharing by list of variables, then only reify
at the end so can merge two cheaps which are then merged.
-}

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
supercompile env = resetTime $ assign $ flatten $ optimise env $ fromJustNote "can't find root" $ env "root"


optimise :: Env -> Exp -> Tree
optimise env = f newHistory
    where  f t x -- | trace (prettyNames x) False = undefined
                 -- | progress t "optimse" = undefined
                 | terminate (<=|) t x = g x (stop t x) t
                 | otherwise = g x (reduce env x) (x += t)
           g x ~(gen,cs) t = {- trace (pretty $ gen (repeat "call")) $ -} Tree x gen (map (f t) cs)


reduce :: Env -> Exp -> ([Var] -> Exp, [Exp])
reduce env = f newHistory
    where f t x -- | progress t "reduce" = undefined
                | terminate (<|) t x = stop t x
                | Just x' <- step env x = f (x += t) x'
                | otherwise = split x


flatten :: Tree -> [Tree]
flatten = nubBy (\x y -> pre x == pre y) . f []
    where f seen t  =  if pre t `elem` seen then [] else
                       t : concatMap (f (pre t:seen)) (children t)


assign :: [Tree] -> [(Var,Exp)]
assign ts = -- error $ pretty $ ($ repeat "?") $ fst $ split $
       --error $ pretty $ fromJust $ lookup "f12" [(b,a) | (a,b) <- names]
       
       -- error $ unlines $ [pretty $ simplify x | (x,y) <- names, y `elem` ["f107","f105"]] 
       
       [(f t, gen t (map f (children t))) |  t <- ts]
    where f t = fromJust $ lookup (pre t) names
          names = zip (map pre ts) freshNames 


freshNames = "root" : ["f" ++ show i | i <- [1..]]


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
    Nothing -> FlatExp free [("_fake",App noname root [])] "_fake"
    Just (v,e) | Just (v2,c2) <- force e -> FlatExp free (("_fake",App noname v2 []):(v,c2 "_fake"):delFst v bind) root
    _ -> flat


force :: Exp -> Maybe (Var, Var -> Exp)
force (App n x y) = Just (x, \x -> App n x y)
force (Case n x y) = Just (x, \x -> Case n x y)
force (Let n x y) = Just (y, Let n x)
force _ = Nothing


---------------------------------------------------------------------
-- BOXES

debox :: Exp -> ([Var] -> Exp, [Exp])
debox x | otherwise = (a,b)
    where (a,b) = deboxName $ deboxFree x


-- name and extract the Box components
deboxName :: Exp -> ([Var] -> Exp, [Exp])
deboxName x = (regen, boxes)
    where
        boxes = nub [y | Box y <- universe x]
        regen names = transform f x
            where vs = zip boxes names
                  f (Box x) = App noname (fromJust $ lookup x vs) []
                  f x = remAppBox x


-- simplify and give sufficient free variables to Box bits
deboxFree :: Exp -> Exp
deboxFree o = transform f o
    where
        fo = free o

        f (Box x) = appBox (simplify $ lams vs x2) vs
            where
                vs = {- sort $ -} fx2 \\ fo
                x2 = simplify x
                fx2 = free x2
        f x = x


appBox :: Exp -> [Var] -> Exp
appBox x vs = Let noname [("_box1",Box x),("_box2",App noname "_box1" vs)] "_box2"

remAppBox :: Exp -> Exp
remAppBox (Let _ [("_box1",App _ x []),("_box2",App _ "_box1" vs)] "_box2") = App noname x vs
remAppBox x = x


---------------------------------------------------------------------
-- SHARING

-- given a set of expressions bound to boxes, you may:
-- * move an expression under a box if it's only used by one
-- * copy an expression under a box if it's cheap
-- the test function must pass on every expression under a box
share :: (Exp -> Bool) -> Exp -> Exp
share test x = fromFlat $ FlatExp vars bind2 root
    where
        FlatExp vars bind root = toFlat x
        norm = filter (not . isBox . snd) bind
        keep = nub $ root : concatMap (free . snd) norm
        boxes = [(v, simplify x) | (v,Box x) <- bind]
        boxes2 = sharer test keep boxes
        bind2 = norm ++ map (second Box) boxes2


data Sharer = Sharer {rank :: Int, var :: Var, vals :: [Var], val :: Exp, fre :: [Var]}

sharer :: (Exp -> Bool) -> [Var] -> [(Var, Exp)] -> [(Var, Exp)]
sharer test keep xs = map (\x -> (var x, val x)) $ once $ cheap
        [mk (length $ filter (== getName b) names) a [a] | (a,b) <- xs]
    where
        mk a b c = Sharer a b c d (free d)
            where d = simplify $ Let noname [(c, fromJust $ lookup c xs) | c <- c] b
        names = map (getName . snd) xs
        order = sortBy (\x y -> compare (rank x) (rank y))

        -- move a single variable wherever you can, delete if no longer needed
        move :: Var -> [Sharer] -> [Sharer]
        move vv xs = if var v `elem` keep || any (elem (var v) . fre) xs2 then xs2
                     else filter ((/=) (var v) . var) xs2
            where
                v = head $ filter ((==) vv . var) xs
                xs2 = map f xs
                f x | var v `elem` fre x, test $ val x2 = x2
                    | otherwise = x
                    where x2 = mk (max (rank v) (rank x)) (var x) (vals x `union` vals v)

        -- merge all the cheap ones
        cheap :: [Sharer] -> [Sharer]
        cheap xs = foldl (flip move) xs $ map var $ filter (isCheap . val) $ order xs

        -- merge all the ones used once
        once :: [Sharer] -> [Sharer]
        once xs = f poss
            where frees = concatMap fre xs
                  poss = [x | x <- order xs, var x `notElem` keep && length (filter (== var x) frees) <= 1]

                  f [] = xs
                  f (p:ps) | length xs2 == length xs = f ps
                           | otherwise = once xs2
                       where xs2 = move (var p) xs


{-



simplifyBox = transform f
    where f (Box x) = Box $ simplify x
          f x = x


-- penalise duplicates as they are more expensive, and usually want splitting
order :: Exp -> [(Var,Exp)] -> [(Var,Exp)]
order x ys = sortOn f ys
    where
        FlatExp vars bind root = toFlat x
        names = map (getName . snd) bind
        f y = length $ filter ((==) $ getName $ fromJust $ lookup (fst y) bind) names

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
-}

isCheap (App _ _ []) = True
isCheap (App _ x xs) | Just n <- arity x = length xs < n
isCheap Con{} = True
isCheap (Let _ bind _) = all (isCheap . snd) bind
isCheap _ = False

---------------------------------------------------------------------
-- OPERATIONS

step :: Env -> Exp -> Maybe Exp
step env x | Just (v,App _ f []) <- stackTop flat, Just e <- env f =
    Just $ simplify $ fromFlat $ FlatExp free ((v,e):delFst v bind) root
    where flat@(FlatExp free bind root) = stackVar $ toFlat x
step env x = Nothing


split :: Exp -> ([Var] -> Exp, [Exp])
split x
    | Nothing <- s = (const x, [])
    | Just (vc, Case n v xs) <- s = 
        let alt (p@(App _ _ []),x) = (p, Box $ Let noname ((vc,x):delFst vc bind) root)
            alt (p,x) = (p, Box $ Let noname ((v,p):bind) root)
        in debox $ lams free $ Case noname v $ map alt xs
    | Just (v, Lam{}) <- s =
        debox $ share (const True) $ fromFlat $ FlatExp free [(a, if a == v then boxlam b else Box b) | (a,b) <- bind] root
    | Just (v, _) <- s =
        debox $ share (const True) $ fromFlat $ FlatExp free [(a, if a == v then b else Box b) | (a,b) <- bind] root
    where
        s = stackTop flat
        flat@(FlatExp free bind root) = toFlat x
        boxlam (Lam n v x) = Lam n v $ boxlam x
        boxlam x = Box $ simplify x


stop :: History -> Exp -> ([Var] -> Exp, [Exp])
stop hist x = if time 10000 then
        error $ "STOP:\n" ++ prettyNames x ++ "\n ==>\n" ++ prettyNames res
        else debox res
    where
        res = share (not . terminate (<=|) hist) $ fromFlat $ FlatExp free (map (second Box) bind) root
        FlatExp free bind root = toFlat x

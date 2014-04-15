{-# LANGUAGE PatternGuards #-}

module Simplify(simplify, simplify2, simplifyProg) where

import Util
import Type
import Control.Applicative
import Control.Arrow
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Generics.Uniplate.Data
import Debug.Trace


simplifyProg :: [(Var,Exp)] -> [(Var,Exp)]
simplifyProg = inlineVars . map (second $ transform simplify)



inlineVars :: [(Var,Exp)] -> [(Var,Exp)]
inlineVars xs = [(v, subst sub e) | (v,e) <- xs]
    where sub = [(v,Var e) | (v,x) <- xs, Just e <- [f x]]
          f (Var x) = g x
          f _ = Nothing
          g v = case lookup v xs of
                    Just x -> f x `mplus` Just v
                    Nothing -> Just v



-- We run relabel 3 times:
-- first time round gives unique names
-- then we simplify (assuming unique names)
-- second time round does GC
-- third time round does variable normalisation
simplify :: Exp -> Exp
simplify = fixEq simplify1

simplify1 x = f "v" $ f "_2" $ runFreshExp "_1" x $ skipLam reduce =<< relabel Map.empty x
    where f s x = runFreshExp s x $ relabel Map.empty x


runFreshExp :: String -> Exp -> Fresh a -> a
runFreshExp v x act = runFresh v $ filterFresh (`notElem` free x) >> act


skipLam :: (Exp -> Fresh Exp) -> Exp -> Fresh Exp
skipLam f (Lam v x) = fmap (Lam v) $ skipLam f x
skipLam f x = f x


simplify2 :: Exp -> Exp
simplify2 = transform f
    where
        f (App (Lam v x) y) = f $ Let [(v,y)] x
        f (Let [] y) = f y
        f (Let vs y) | (sub@(_:_),keep) <- partition (cheap . snd) vs = simplify2 $ Let keep $ subst sub y
        f (Case (Case on alts1) alts2) = simplify2 $ Case on [(a,Case c alts2) | (a,c) <- alts1]
        f (Case (Con ctr vs) alts) = simplify2 $ head $ mapMaybe g alts
            where g (PCon c xs, y) | c == ctr = Just $ Let (zip xs vs) y
                  g (PWild, y) = Just y
                  g _ = Nothing
        f x = x

linear v y = False

cheap (Var _) = True
cheap (Con _ _) = True
cheap (Lam _ _) = True
cheap _ = True -- False -- WRONG, just temporary




---------------------------------------------------------------------
-- RELABEL

-- relabel, normalise the order of let bindings, and do a GC pass over unused let expressions
relabel :: Map.Map Var Var -> Exp -> Fresh Exp
relabel ren (Lam v x) = do
    v2 <- fresh
    fmap (Lam v2) $ relabel (Map.insert v v2 ren) x

-- let does the actual GC and reorder
relabel ren (Let xs v) = do
    xs <- return xs -- $ rebind v xs
    vs2 <- freshN $ length xs
    ren <- return $ Map.fromList (zip (map fst xs) vs2) `Map.union` ren
    v2 <- relabel ren v
    xs2 <- mapM (f ren) (zip vs2 $ map snd xs)
    return $ Let xs2 v2  
    where
        f ren (v,x) = fmap ((,) v) $ relabel ren x

relabel ren (Case v xs) = do
    Case <$> relabel ren v <*> mapM (f ren) xs
    where
        f ren (PCon c vs,b) = do
            vs2 <- freshN $ length vs
            ren <- return $ Map.fromList (zip vs vs2) `Map.union` ren
            fmap ((,) $ PCon c vs2) $ relabel ren b
        f ren (PWild,b) = do
            fmap ((,) PWild) $ relabel ren b

relabel ren (App v1 v2) = App <$> relabel ren v1 <*> relabel ren v2
relabel ren (Con c vs) = Con c <$> mapM (relabel ren) vs
relabel ren (Var v) = return $ Var $ Map.findWithDefault v v ren

relabelVar ren v = Map.findWithDefault v v ren


-- put in order, and do a GC
rebind :: Var -> [(Var,Exp)] -> [(Var,Exp)]
rebind v xs | v `notElem` map fst xs = []
            | otherwise = [(a, fromJustNote "rebind" $ lookup a xs) | a <- order [] $ need [v]]
    where
        -- (a,b) means a relies on b
        pairs = [(a,b) | (a,b) <- xs, b <- free b, b `elem` map fst xs]

        need seen = if null next then seen else need (seen++next)
            where next = [b | (a,b) <- pairs, a `elem` seen, b `notElem` seen]

        order done [] = done
        order done todo = if null a then error "rebind circle" else order (done++a) b
            where (a,b) = partition f todo
                  f x = all (\(a,b) -> a /= x || b `elem` done) pairs


---------------------------------------------------------------------
-- REDUCE




-- reduce, assuming that let variables are sorted by order of use
-- i.e. will only depend on variables defined after you
reduce :: Exp -> Fresh Exp
{-
reduce (Let xs v) = do
    xs2 <- f [] xs
    return $ case lookup v xs2 of
        Just (App v2 []) -> Let xs2 v2
        _ -> Let xs2 v
    where
        f :: [(Var,Exp)] -> [(Var,Exp)] -> Fresh [(Var,Exp)]
        f done ((v,e):odo)
            | App v2 [] <- e =
                let g xs = [(a,subst [(v,v2)] b) | (a,b) <- xs]
                in f ((v,e) : g done) (g odo) -- FIXME: This stage may not generate a fixed point!!!
            | Let xs v2 <- e = f done (reverse xs ++ [(v,App v2 [])] ++ odo)
            | App v1 (v2:vs) <- e, Just e2@Lam{} <- lookup v1 done = do
                Lam v3 e3 <- relabel Map.empty e2
                v4 <- fresh
                f done ((v4, subst [(v3,v2)] e3):(v,App v4 vs):odo)
            | App v1 v2 <- e, Just e2@(Con c vs) <- lookup v1 done = do
                f done ((v,Con c (vs++v2)):odo)
            | App v1 v2 <- e, Just e2@(App v3 vs) <- lookup v1 done, Just a <- arity v3, a >= length (v2 ++ vs) =
                f done ((v,App v3 (vs++v2)):odo)
            | Case v2 alts <- e, Just (Con _ c vs) <- lookup v2 done =
                let pick (Con c2 vs2, x) | c == c2 = [subst (zip vs2 vs) x]
                    pick (App vm [], x) | c `notElem` smallCtors = [subst [(vm,v2)] x]
                    pick _ = []
                    r = head $ concatMap pick alts ++ error
                            ("confused, no case match...\n" ++ pretty (Con c vs) ++ "\n" ++ pretty e)
                in f done ((v,r):odo)
            | otherwise = f ((v,e):done) odo
        f done [] = return done
-}

reduce x = return x



smallCtors = ["Nothing","Just","True","False","(:)","[]",":"]


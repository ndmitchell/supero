{-# LANGUAGE PatternGuards, GeneralizedNewtypeDeriving, TupleSections, ViewPatterns #-}

module Supercompile(supercompile) where

import Exp hiding (equivalent)
import Simplify
import Util hiding (fresh)
import Data.List
import Data.Maybe
import Control.Arrow
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Generics.Uniplate.Data hiding (children)
import Control.Applicative
import Control.Exception
import System.IO.Unsafe


---------------------------------------------------------------------
-- MONAD

type S a = StateT [(Var, Exp, Exp)] IO a

debug :: String -> S ()
debug = liftIO . putStrLn


equivalent = equivalentOn (relabel . eval . relabel . transform f)
    where f (Var (V i)) | i == "jail" || i == "define" = Lam (V "v") $ Var $ V "v"
          f x = x

---------------------------------------------------------------------
-- MANAGER

supercompile :: [(Var,Exp)] -> [(Var,Exp)]
supercompile env = resetTime $ unsafePerformIO $ flip evalStateT [] $ do
    res <- define env $ fromJustNote "Could not find root in env" $ lookup (V "root") env
    s <- get
    return $ (V "root",res) : reverse [(a,b) | (a,_,b) <- s]


define :: [(Var,Exp)] -> Exp -> S Exp
define env x = do
    s <- get
    x <- return $ relabel $ simplify x
    name <- case find (\(_,t,_) -> t == x) s of
        Just (name,_,_) -> return name
        Nothing -> do
            let name = V $ "_" ++ show (length s + 1)
            debug $ "define: " ++ fromVar name ++ " = " ++ pretty x
            -- liftIO getLine
            modify ((name,x,Var $ V "undefined"):)
            bod <- optimise env x
            modify $ map $ \o@(name2,t,_) -> if name == name2 then (name,t,bod) else o
            return name
    return $ Var $ name


optimise :: [(Var,Exp)] -> Exp -> S Exp
optimise env (simplify -> x)
    | V "jail" `elem` universeBi x = dejail env x
    | Just x <- unfold env x = optimise env x
    | otherwise = do debug $ "peel: " ++ pretty x; peel env $ simplify x


dejail :: [(Var,Exp)] -> Exp -> S Exp
dejail env (fromLams -> (root, x)) = do
        debug $ "dejail: " ++ pretty x
        (bod,(_,(unzip -> (vs,xs)))) <- return $ runState (f [] x) (fresh $ vars x, [])
        -- debug $ "dejail out: " ++ pretty (lams root $ lets (zip vs xs) bod)
        let def x = let fv = root `intersect` free x in flip apps (map Var fv) <$> define env (lams fv x)
        liftIO $ evaluate $
            equivalent "dejail"
            (lams root x)
            (lams root (apps (lams vs bod) xs))
        lams root <$> (apps <$> def (lams vs bod) <*> mapM def xs)
    where
        f :: [Var] -> Exp -> State ([Var], [(Var,Exp)]) Exp
        f vs (Lam v x) = Lam v <$> f (vs++[v]) x
        f vs (Case v xs) = Case <$> f vs v <*> mapM (g vs) xs
        f vs (Let v x y) = Let v <$> f vs x <*> f (vs++[v]) y
        f vs (App (Var (V "jail")) x) = do
                let vs2 = reverse $ nub $ reverse $ vs `intersect` free x
                (n:ew,bnd) <- get
                case rlookup (lams vs2 x) bnd of
                    Just n -> return $ apps (Var n) $ map Var vs2
                    Nothing -> do
                        put (ew,(n,lams vs2 x):bnd)
                        return $ apps (Var n) $ map Var vs2
        f vs (App x y) = App <$> f vs x <*> f vs y
        f vs x = return x

        g vs (PWild, x) = (PWild,) <$> f vs x
        g vs (PCon c ps, x) = (PCon c ps,) <$> f (vs ++ ps) x


peel :: [(Var,Exp)] -> Exp -> S Exp
peel env = f [] False
    where
        f vs down (Lam v x) = Lam v <$> f (vs++[v]) down x
        f vs down (fromApps -> (Con c, xs)) = apps (Con c) <$> mapM (f vs True) xs
        f vs down (fromApps -> (Var v, xs)) | v `elem` vs || isNothing (lookup v env) = apps (Var v) <$> mapM (f vs True) xs
        f vs False (Case v xs) = Case <$> f vs True v <*> mapM (g vs) xs
        f vs False (App x y) = App <$> f vs True x <*> f vs True y
        f vs False (Let v x y) = Let v <$> f vs True x <*> f (vs++[v]) True y
        f vs down x = flip apps (map Var vs2) <$> define env (lams vs2 x)
            where vs2 = reverse $ nub $ reverse $ vs `intersect` free x

        g vs (PWild, x) = (PWild,) <$> f vs True x
        g vs (PCon c ps, x) = (PCon c ps,) <$> f (vs ++ ps) True x


unfold :: [(Var,Exp)] -> Exp -> Maybe Exp
unfold env x = case x of
    Var v -> lookup v env
    Lam v x -> Lam v <$> f x
    App x y -> flip App y <$> f x
    Let a b y -> Let a b <$> f y
    Case x y -> flip Case y <$> f x
    _ -> Nothing
    where f = unfold env

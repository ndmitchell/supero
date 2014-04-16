{-# LANGUAGE DeriveDataTypeable, PatternGuards, TupleSections #-}

module Exp(
    Var(..), Con(..), Exp(..), Pat(..),
    fromApps, lets, lams, apps,
    prettys, pretty,
    vars, free, subst, relabel, count, linear,
    fromHSE, toHSE
    ) where


import Data.Maybe
import Data.List
import Data.Data
import Control.Applicative
import Control.Monad.State
import Data.Char
import Control.Arrow
import Language.Haskell.Exts hiding (Exp,Name,Pat,Var,Let,App,Case,Con,name)
import qualified Language.Haskell.Exts as H
import HSE
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map


---------------------------------------------------------------------
-- TYPE

newtype Var = V {fromVar :: String} deriving (Data,Typeable,Eq,Show)
newtype Con = C {fromCon :: String} deriving (Data,Typeable,Eq,Show)

data Exp
    = Var Var
    | Con Con
    | App Exp Exp
    | Let Var Exp Exp -- non-recursive
    | Lam Var Exp
    | Case Exp [(Pat,Exp)]
      deriving (Data,Typeable,Show,Eq)

data Pat
    = PCon Con [Var]
    | PWild
      deriving (Data,Typeable,Show,Eq)

apps x (y:ys) = apps (App x y) ys
apps x [] = x

lams (y:ys) x = Lam y $ lams ys x
lams [] x = x

lets [] x = x
lets ((a,b):ys) x = Let a b $ lets ys x


isVar (Var _) = True; isVar _ = False
isCon (Con _) = True; isCon _ = False


fromApps (App x y) = (a,b ++ [y])
    where (a,b) = fromApps x
fromApps x = (x,[])

pretty :: Exp -> String
pretty = prettyPrint . toExp

prettys :: [(Var,Exp)] -> String
prettys = prettyPrint . toHSE


---------------------------------------------------------------------
-- BINDING AWARE OPERATIONS

vars :: Exp -> [Var]
vars = universeBi

varsP :: Pat -> [Var]
varsP = universeBi

free :: Exp -> [Var]
free (Var x) = [x]
free (App x y) = nub $ free x ++ free y
free (Lam x y) = delete x $ free y
free (Case x y) = nub $ free x ++ concat [free b \\ varsP a | (a,b) <- y]
free (Let a b y) = nub $ free b ++ delete a (free y)
free _ = []

subst :: [(Var,Exp)] -> Exp -> Exp
subst [] x = x
subst ren e = case e of
    Var x -> fromMaybe (Var x) $ lookup x ren
    App x y -> App (f [] x) (f [] y)
    Lam x y -> Lam x (f [x] y)
    Case x y -> Case (f [] x) [(a, f (varsP a) b) | (a,b) <- y]
    Let a b y -> Let a (f [] b) $ f [a] y
    x -> x
    where f del x = subst (filter (flip notElem del . fst) ren) x

linear :: Var -> Exp -> Bool
linear v x = count v x <= 1

count :: Var -> Exp -> Int
count v (Var x) = if v == x then 1 else 0
count v (Lam w y) = if v == w then 0 else count v y
count v (Let w x y) = count v x + (if v == w then 0 else count v y)
count v (Case x alts) = count v x + maximum [if v `elem` varsP p then 0 else count v c | (p,c) <- alts]
count v (App x y) = count v x + count v y
count v _ = 0

relabel :: Exp -> Exp
relabel x = evalState (f x) (fresh \\ free x)
    where
        f :: Exp -> State [Var] Exp
        f (Lam v x) = do i <- var; Lam i <$> f (subst [(v,Var i)] x)
        f (Let v x y) = do i <- var; Let i <$> f x <*> f (subst [(v,Var i)] y)
        f (Case x alts) = Case x <$> mapM g alts
        f (App x y) = App <$> f x <*> f y
        f x = return x

        g (PWild, x) = (PWild,) <$> f x
        g (PCon c vs, x) = do is <- replicateM (length vs) var; (PCon c is,) <$> f (subst (zip vs $ map Var is) x)

        var = do s:ss <- get; put ss; return s

        fresh = map V $ concatMap new [1..]
        new 1 = map return ['a'..'z']
        new i = [a ++ b | a <- new 1, b <- new (i-1)]

---------------------------------------------------------------------
-- FROM HSE

fromHSE :: Module -> [(Var,Exp)]
fromHSE m = concatMap fromDecl xs
  where Module _ _ _ _ _ _ xs = deflate m

fromDecl :: Decl -> [(Var,Exp)]
fromDecl (PatBind _ (PVar f) Nothing (UnGuardedRhs x) (BDecls [])) = [(V $ fromName f, fromExp x)]
fromDecl TypeSig{} = []
fromDecl DataDecl{} = []
fromDecl TypeDecl{} = []
fromDecl x = error $ "Unhandled fromDecl: " ++ show x

fromExp :: H.Exp -> Exp
fromExp (Lambda _ [PVar (Ident x)] bod) = Lam (V x) $ fromExp bod
fromExp (H.App x y) = App (fromExp x) (fromExp y)
fromExp (H.Var (UnQual x)) = Var $ V $ fromName x
fromExp (H.Con (UnQual x)) = Con $ C $ fromName x
fromExp (Paren x) = fromExp x
fromExp (H.Case x xs) = Case (fromExp x) $ map fromAlt xs
fromExp (H.Let (BDecls [d]) x) | [(a,b)] <- fromDecl d =  Let a b $ fromExp x
fromExp x = error $ "Unhandled fromExp: " ++ show x

fromName :: H.Name -> String
fromName (Ident x) = x
fromName (Symbol x) = x

fromAlt :: Alt -> (Pat, Exp)
fromAlt (Alt _ pat (UnGuardedAlt bod) (BDecls [])) = (fromPat pat, fromExp bod)
fromAlt x = error $ "Unhandled fromAlt: " ++ show x

fromPat :: H.Pat -> Pat
fromPat (PParen x) = fromPat x
fromPat (PApp (UnQual c) xs) = PCon (C $ fromName c) $ map (V . fromPatVar) xs
fromPat PWildCard = PWild
fromPat x = error $ "Unhandled fromPat: " ++ show x

fromPatVar :: H.Pat -> String
fromPatVar (PVar x) = fromName x
fromPatVar x = error $ "Unhandled fromPatVar: " ++ show x


---------------------------------------------------------------------
-- TO HSE

toHSE :: [(Var,Exp)] -> Module
toHSE xs = inflate $ Module sl (ModuleName "") [] Nothing Nothing [] $ map (uncurry toDecl) xs

toDecl :: Var -> Exp -> Decl
toDecl (V f) x = PatBind sl (PVar $ toName f) Nothing (UnGuardedRhs $ toExp x) (BDecls [])

toExp :: Exp -> H.Exp
toExp (Var (V x)) = H.Var $ UnQual $ toName x
toExp (Con (C x)) = H.Con $ UnQual $ toName x
toExp (Lam (V x) y) = Lambda sl [PVar $ toName x] $ toExp y
toExp (Let a b y) = H.Let (BDecls [toDecl a b]) $ toExp y
toExp (App x y) = H.App (toExp x) (toExp y)
toExp (Case x y) = H.Case (toExp x) (map toAlt y)

toAlt :: (Pat, Exp) -> Alt
toAlt (x,y) = Alt sl (toPat x) (UnGuardedAlt $ toExp y) (BDecls [])

toPat :: Pat -> H.Pat
toPat (PCon (C c) vs) = PApp (UnQual $ toName c) (map (PVar . Ident . fromVar) vs)
toPat PWild = PWildCard

toName :: String -> H.Name
toName xs@(x:_) | isAlphaNum x || x `elem` "'_(" = Ident xs
                | otherwise = Symbol xs

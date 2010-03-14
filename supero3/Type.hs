{-# LANGUAGE DeriveDataTypeable #-}

module Type(
    Var, Con, Exp(..), Pat, pretty, vars, free, subst,
    FlatExp(..), toFlat, fromFlat, lams,
    Name, noname, incName,
    Env(..), env,
    fromHSE, toHSE
    ) where


import Data.Maybe
import Data.List
import Data.Data
import Data.Char
import Language.Haskell.Exts hiding (Exp,Name,Pat,Var,Let,App,Case,Con,name)
import qualified Language.Haskell.Exts as H
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map

---------------------------------------------------------------------
-- TYPE

type Var = String
type Con = String

data Name = Name Var Int Int deriving (Data,Typeable,Eq)

noname = Name "<no name>" 0 0

instance Show Name where
    show x | x == noname = "_"
    show (Name x y z) = "<"++x++","++show y++","++show z++">"

incName (Name a b c) = Name a b (c+1)


data Exp = Var  Name Var
         | Con  Name Con [Var]
         | App  Name Var Var
         | Lam  Name Var Exp
         | Case Name Var [(Pat,Exp)]
         | Let  Name [(Var,Exp)] Var
         | Box  Exp -- to represent <? ?> brackets
           deriving (Data,Typeable,Show)

instance Eq Exp where
    x == y = toExp x == toExp y -- ignore names

pretty :: Exp -> String
pretty = prettyPrint . toExp


type Pat = Exp


type Env = Var -> Maybe Exp


env :: [(Var,Exp)] -> Env
env xs = \x -> Map.lookup x mp
    where mp = Map.fromList xs


vars :: Exp -> [Var]
vars (Var _ x) = [x]
vars (Con _ _ xs) = xs
vars (App _ x y) = [x,y]
vars (Lam _ x y) = x : vars y
vars (Case _ x y) = x : concat [vars a ++ vars b | (a,b) <- y]
vars (Let _ x y) = concat [a : vars b | (a,b) <- x] ++ [y]
vars (Box x) = vars x


free :: Exp -> [Var]
free (Var _ x) = [x]
free (Con _ _ xs) = nub xs
free (App _ x y) = nub [x,y]
free (Lam _ x y) = delete x $ free y
free (Case _ x y) = nub $ x : concat [free b \\ vars a | (a,b) <- y]
free (Let _ x y) = nub (concatMap (free . snd) x ++ [y]) \\ map fst x
free (Box x) = free x


subst :: [(Var,Var)] -> Exp -> Exp
subst [] x = x
subst ren e = case e of
    Var n v -> Var n $ f v
    Con n c vs -> Con n c $ map f vs
    App n x y -> App n (f x) (f y)
    Lam n x y -> Lam n x (g [x] y)
    Case n x y -> Case n (f x) [(a, g (vars a) b) | (a,b) <- y]
    Let n x y -> Let n [(a,g (map fst x) b) | (a,b) <- x] $ if y `elem` map fst x then y else f y
    Box x -> g [] x
    where
        f x = fromMaybe x $ lookup x ren
        g del x = subst (filter (flip notElem del . fst) ren) x


---------------------------------------------------------------------
-- FLAT TYPE

data FlatExp = FlatExp [Var] [(Var,Exp)] Var

toFlat :: Exp -> FlatExp
toFlat = f []
    where
        f vs (Lam _ v x) = f (vs++[v]) x
        f vs (Let _ xs y) = FlatExp vs xs y
        f vs x = FlatExp vs [("_flat",x)] "_flat"


fromFlat :: FlatExp -> Exp
fromFlat (FlatExp vs x y) = lams vs $ Let noname x y

lams [] x = x
lams (l:ls) x = Lam noname l $ lams ls x


---------------------------------------------------------------------
-- FROM HSE

fromHSE :: Module -> [(Var,Exp)]
fromHSE (Module _ _ _ _ _ _ xs) = [(f, assignNames f x) | (f,x) <- map fromDecl xs]


fromDecl :: Decl -> (Var,Exp)
fromDecl (PatBind _ (PVar (Ident f)) Nothing (UnGuardedRhs x) (BDecls [])) = (f, fromExp x)
fromDecl (FunBind [Match _ (Ident f) vars Nothing (UnGuardedRhs x) (BDecls [])]) = (f, fromExp $ Lambda sl vars x)
fromDecl x = error $ "Unhandled fromDecl: " ++ show x


fromExp :: H.Exp -> Exp
fromExp (Lambda _ [] x) = fromExp x
fromExp (Lambda _ (PVar (Ident x):vars) bod) = Lam noname x $ fromExp $ Lambda sl vars bod
fromExp o@(H.App x y) = Let noname [(f1,fromExp x),(f2,fromExp y),(f3,App noname f1 f2)] f3
    where f1:f2:f3:_ = freshNames o
fromExp (H.Var (UnQual (Ident x))) = Var noname x
fromExp (Paren x) = fromExp x
fromExp o@(H.Case x xs) = Let noname [(f1,fromExp x),(f2,Case noname f1 $ map fromAlt xs)] f2
    where f1:f2:_ = freshNames o
fromExp (List []) = Con noname "[]" []
fromExp o@(InfixApp x (QConOp (Special Cons)) y) = Let noname [(f1,fromExp x),(f2,fromExp y),(f3,Con noname ":" [f1,f2])] f3
    where f1:f2:f3:_ = freshNames o
fromExp x = error $ "Unhandled fromExp: " ++ show x


fromAlt :: Alt -> (Pat, Exp)
fromAlt (Alt _ pat (UnGuardedAlt bod) (BDecls [])) = (fromPat pat, fromExp bod)
fromAlt x = error $ "Unhandled fromAlt: " ++ show x


fromPat :: H.Pat -> Pat
fromPat (PList []) = Con noname "[]" []
fromPat (PApp (Special Cons) [PVar (Ident x), PVar (Ident y)]) = Con noname ":" [x,y]
fromPat (PInfixApp a b c) = fromPat $ PApp b [a,c]
fromPat x = error $ "Unhandled fromPat: " ++ show x


freshNames :: H.Exp -> [String]
freshNames x  = ['v':show i | i <- [1..]] \\ [y | Ident y <- universeBi x]


-- Fixup: Assign names properly
assignNames :: Var -> Exp -> Exp
assignNames _ x = x

---------------------------------------------------------------------
-- TO HSE

toHSE :: [(Var,Exp)] -> Module
toHSE xs = Module sl (ModuleName "") [] Nothing Nothing [] $ map toDecl xs

toDecl :: (Var,Exp) -> Decl
toDecl (f,x) = PatBind sl (PVar $ Ident f) Nothing (UnGuardedRhs $ toExp x) (BDecls [])

toExp :: Exp -> H.Exp
toExp (Var _ x) = toVar x
toExp (Lam _ x y) = Paren $ lambda [PVar $ Ident x] $ toExp y
toExp (Let _ xs y) = Paren $ H.Let (BDecls $ map toDecl xs) $ toVar y
toExp (App _ x y) = Paren $ H.App (toVar x) (toVar y)
toExp (Case _ x y) = Paren $ H.Case (toVar x) (map toAlt y)
toExp (Con _ c vs) = Paren $ foldl H.App (H.Con $ UnQual $ toName c) (map toVar vs)
toExp (Box x) = BracketExp $ ExpBracket $ toExp x
toExp x = error $ "toExp, todo: " ++ show x

toAlt :: (Pat, Exp) -> Alt
toAlt (x,y) = Alt sl (toPat x) (UnGuardedAlt $ toExp y) (BDecls [])

toPat :: Pat -> H.Pat
toPat (Con _ c vs) = PApp (UnQual $ toName c) (map (PVar . Ident) vs)
toPat x = error $ "toPat, todo: " ++ show x

toVar :: Var -> H.Exp
toVar x = H.Var $ UnQual $ toName x

toName :: String -> H.Name
toName x | x == ":" = Symbol x
         | otherwise = Ident x

sl = SrcLoc "" 0 0


lambda v1 (Lambda _ v2 x) = Lambda sl (v1++v2) x
lambda v1 (Paren x) = lambda v1 x
lambda v1 x = Lambda sl v1 x

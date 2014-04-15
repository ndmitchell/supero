{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

module Type(
    Var, Con, Exp(..), Pat(..), pretty,
    vars, free, subst, arity, valid, validId,
    FlatExp(..), toFlat, fromFlat, lams, apps,
    Env, env,
    fromHSE, toHSE
    ) where


import Data.Maybe
import Data.List
import Data.Data
import Control.Monad.State
import Data.Char
import Control.Arrow
import Language.Haskell.Exts hiding (Exp,Name,Pat,Var,Let,App,Case,Con,name)
import qualified Language.Haskell.Exts as H
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map

---------------------------------------------------------------------
-- TYPE

type Var = String
type Con = String


data Exp = Con  Con [Exp]
         | App  Exp Exp
         | Var  Var
         | Lam  Var Exp
         | Case Exp [(Pat,Exp)]
         | Let  [(Var,Exp)] Exp
           deriving (Data,Typeable,Show)

instance Eq Exp where
    x == y = toExp x == toExp y -- ignore names

pretty :: Exp -> String
pretty = prettyPrint . toExp


data Pat = PCon Con [Var] | PWild deriving (Data,Typeable,Show)


type Env = Var -> Maybe Exp

arity :: Var -> Maybe Int
arity x | '\'':xs <- dropWhile (/= '\'') x = Just $ read xs
        | otherwise = Nothing


env :: [(Var,Exp)] -> Env
env xs = flip Map.lookup mp
    where mp = Map.fromList xs


vars :: Exp -> [Var]
vars (Var x) = [x]
vars (Con _ xs) = concatMap vars xs
vars (App x y) = vars x ++ vars y
vars (Lam x y) = x : vars y
vars (Case x y) = vars x ++ concat [varsP a ++ vars b | (a,b) <- y]
vars (Let x y) = concat [a : vars b | (a,b) <- x] ++ vars y

varsP :: Pat -> [Var]
varsP (PCon _ xs) = xs
varsP PWild = []

free :: Exp -> [Var]
free (Var x) = [x]
free (Con _ xs) = nub $ concatMap free xs
free (App x y) = nub $ free x ++ free y
free (Lam x y) = delete x $ free y
free (Case x y) = nub $ free x ++ concat [free b \\ varsP a | (a,b) <- y]
free (Let x y) = nub (concatMap (free . snd) x ++ free y) \\ map fst x


subst :: [(Var,Exp)] -> Exp -> Exp
subst [] x = x
subst ren e = case e of
    Var x -> fromMaybe (Var x) $ lookup x ren
    Con c vs -> Con c $ map (g []) vs
    App x y -> App (g [] x) (g [] y)
    Lam x y -> Lam x (g [x] y)
    Case x y -> Case (g [] x) [(a, g (varsP a) b) | (a,b) <- y]
    Let x y -> Let [(a,g (map fst x) b) | (a,b) <- x] $ g (map fst x) y
    where
        g del x = subst (filter (flip notElem del . fst) ren) x


valid :: Exp -> Bool
valid = all (isJust . arity) . free

validId :: Exp -> Exp
validId x | valid x = x
          | otherwise = error $ "Invalid expression:\n" ++ pretty x


---------------------------------------------------------------------
-- FLAT TYPE

data FlatExp = FlatExp [Var] [(Var,Exp)] Var

toFlat :: Exp -> FlatExp
toFlat = f []
    where
        f vs (Lam v x) = f (vs++[v]) x
        -- f vs (Let xs y) = FlatExp vs xs y
        f vs x = FlatExp vs [("_flat",x)] "_flat"


fromFlat :: FlatExp -> Exp
fromFlat (FlatExp vs x y) = lams vs $ Let x $ Var y

lams [] x = x
lams (l:ls) x = Lam l $ lams ls x

apps f xs = foldl App f xs


---------------------------------------------------------------------
-- FROM HSE

fromHSE :: Module -> [(Var,Exp)]
fromHSE (Module _ _ _ _ _ _ xs) = assignArities [(f, x) | (f,x) <- concatMap fromDecl xs]


fromDecl :: Decl -> [(Var,Exp)]
fromDecl (PatBind _ (PVar f) Nothing (UnGuardedRhs x) (BDecls [])) = [(fromName f, fromExp x)]
fromDecl (FunBind [Match _ f vars Nothing (UnGuardedRhs x) (BDecls [])]) = [(fromName f, fromExp $ Lambda sl vars x)]
fromDecl TypeSig{} = []
fromDecl DataDecl{} = []
fromDecl TypeDecl{} = []
fromDecl x = error $ "Unhandled fromDecl: " ++ show x


fromExp :: H.Exp -> Exp
fromExp (Lambda _ [] x) = fromExp x
fromExp (Lambda _ (PVar (Ident x):vars) bod) = Lam x $ fromExp $ Lambda sl vars bod
fromExp o@(H.App x y) = App (fromExp x) (fromExp y)
fromExp (H.Var (UnQual x)) = Var (fromName x)
fromExp (H.Con (UnQual x)) = Con (fromName x) []
fromExp (H.Con (Special Cons)) = Con ":" []
fromExp (LeftSection x (QVarOp y)) = fromExp $ H.App (H.Var y) x
fromExp (Paren x) = fromExp x
fromExp o@(H.Case x xs) = Case (fromExp x) $ map fromAlt xs
fromExp (List []) = Con "[]" []
fromExp (List [x]) = fromExp $ InfixApp x (QConOp (Special Cons)) $ List []
fromExp o@(InfixApp x (QConOp (Special Cons)) y) = Con ":" [fromExp x, fromExp y]
fromExp o@(InfixApp a (QVarOp b) c) = fromExp $ H.App (H.App (H.Var b) a) c
fromExp (Lit x) = Con (prettyPrint x) []
fromExp x@(NegApp _) = Con (prettyPrint x) []
fromExp (If a b c) = fromExp $ H.Case a [f "True" b, f "False" c]
    where f con x = Alt sl (PApp (UnQual $ Ident con) []) (UnGuardedAlt x) (BDecls [])
fromExp o@(H.Let (BDecls xs) x) = Let (concatMap fromDecl xs) $ fromExp x
fromExp (Tuple _ xs) = Con (fromTuple xs) (map fromExp xs)
fromExp (H.Con (Special (TupleCon _ n))) = Con (fromTuple $ replicate n ()) []
fromExp x = error $ "Unhandled fromExp: " ++ show x


fromName :: H.Name -> String
fromName (Ident x) = x
fromName (Symbol x) = x

fromAlt :: Alt -> (Pat, Exp)
fromAlt (Alt _ pat (UnGuardedAlt bod) (BDecls [])) = (fromPat pat, fromExp bod)
fromAlt x = error $ "Unhandled fromAlt: " ++ show x

fromPat :: H.Pat -> Pat
fromPat (PParen x) = fromPat x
fromPat (PList []) = PCon "[]" []
fromPat (PApp (Special Cons) xs) = PCon ":" $ map fromPatVar xs
fromPat (PInfixApp a b c) = fromPat $ PApp b [a,c]
fromPat (PApp (UnQual c) xs) = PCon (fromName c) $ map fromPatVar xs
fromPat (PTuple _ xs) = PCon (fromTuple xs) $ map fromPatVar xs
fromPat (PApp (Special (TupleCon _ n)) xs) = PCon (fromTuple xs) $ map fromPatVar xs
fromPat PWildCard = PWild
fromPat x = error $ "Unhandled fromPat: " ++ show x

fromTuple xs = "(" ++ replicate (length xs - 1) ',' ++ ")"

fromPatVar :: H.Pat -> String
fromPatVar (PVar x) = fromName x
fromPatVar x = error $ "Unhandled fromPatVar: " ++ show x


freshNames :: H.Exp -> [String]
freshNames x  = ['v':show i | i <- [1..]] \\ [y | Ident y <- universeBi x]


-- Fixup: Move arity information
assignArities :: [(Var,Exp)] -> [(Var,Exp)]
assignArities xs = checkPrims $ ("root",Var $ fromJust $ lookup "root" ren) :
                                [(fromJust $ lookup a ren, subst (map (second Var) ren) b) | (a,b) <- xs]
    where ren = [(a, a ++ "'" ++ show (f b)) | (a,b) <- xs]
          f (Lam _ x) = 1 + f x
          f _ = 0


checkPrims :: [(Var,Exp)] -> [(Var,Exp)]
checkPrims xs | null bad = xs
              | otherwise = error $ "checkPrims failed: " ++ show bad
    where
        bad = nub [v | (_,x) <- xs, v <- free x, Nothing <- [arity v]]


---------------------------------------------------------------------
-- TO HSE

toHSE :: [(Var,Exp)] -> Module
toHSE xs = Module sl (ModuleName "") [] Nothing Nothing [] $ map toDecl xs

toDecl :: (Var,Exp) -> Decl
toDecl (f,x) = PatBind sl (PVar $ Ident f) Nothing (UnGuardedRhs $ toExp x) (BDecls [])

toExp :: Exp -> H.Exp
toExp (Var x) = H.Var $ UnQual $ Ident x
toExp (Lam x y) = Paren $ lambda [PVar $ Ident x] $ toExp y
toExp (Let xs y) = Paren $ H.Let (BDecls $ map toDecl xs) $ toExp y
toExp (App x y) = Paren $ H.App (toExp x) (toExp y)
toExp (Case x y) = Paren $ H.Case (toExp x) (map toAlt y)
toExp (Con c vs) = Paren $ foldl H.App (H.Con $ UnQual $ toName c) (map toExp vs)

toAlt :: (Pat, Exp) -> Alt
toAlt (x,y) = Alt sl (toPat x) (UnGuardedAlt $ toExp y) (BDecls [])

toPat :: Pat -> H.Pat
toPat (PCon c vs) = PApp (UnQual $ toName c) (map (PVar . Ident) vs)
toPat PWild = PWildCard

toVar :: Var -> H.Exp
toVar x = H.Var $ UnQual $ toName x

toName :: String -> H.Name
toName xs@(x:_) | isAlphaNum x || x `elem` "'_(" = Ident xs
                | otherwise = Symbol xs

sl = SrcLoc "" 0 0


lambda v1 (Lambda _ v2 x) = Lambda sl (v1++v2) x
lambda v1 (Paren x) = lambda v1 x
lambda v1 x = Lambda sl v1 x

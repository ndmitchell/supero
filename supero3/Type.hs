{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

module Type(
    Var, Con, Exp(..), Pat, pretty, isBox,
    vars, free, subst, arity,
    FlatExp(..), toFlat, fromFlat, lams,
    Name, noname, prettyNames, getName,
    Env(..), env,
    fromHSE, toHSE
    ) where


import Data.Maybe
import Data.List
import Data.Data
import Control.Monad.State
import Data.Char
import Language.Haskell.Exts hiding (Exp,Name,Pat,Var,Let,App,Case,Con,name)
import qualified Language.Haskell.Exts as H
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map

---------------------------------------------------------------------
-- TYPE

type Var = String
type Con = String


data Exp = Con  Name Con [Var]
         | App  Name Var [Var]
         | Lam  Name Var Exp
         | Case Name Var [(Pat,Exp)]
         | Let  Name [(Var,Exp)] Var
         | Box  Exp -- to represent <? ?> brackets
           deriving (Data,Typeable,Show)

instance Eq Exp where
    x == y = toExp x == toExp y -- ignore names

pretty :: Exp -> String
pretty = prettyPrint . toExp

isBox Box{} = True ; isBox _ = False


type Pat = Exp


type Env = Var -> Maybe Exp

arity :: Var -> Maybe Int
arity x | '\'':xs <- dropWhile (/= '\'') x = Just $ read xs
        | otherwise = Nothing


env :: [(Var,Exp)] -> Env
env xs = \x -> Map.lookup x mp
    where mp = Map.fromList xs


vars :: Exp -> [Var]
vars (Con _ _ xs) = xs
vars (App _ x xs) = x:xs
vars (Lam _ x y) = x : vars y
vars (Case _ x y) = x : concat [vars a ++ vars b | (a,b) <- y]
vars (Let _ x y) = concat [a : vars b | (a,b) <- x] ++ [y]
vars (Box x) = vars x


free :: Exp -> [Var]
free (Con _ _ xs) = nub xs
free (App _ x xs) = nub $ x:xs
free (Lam _ x y) = delete x $ free y
free (Case _ x y) = nub $ x : concat [free b \\ vars a | (a,b) <- y]
free (Let _ x y) = nub (concatMap (free . snd) x ++ [y]) \\ map fst x
free (Box x) = free x


subst :: [(Var,Var)] -> Exp -> Exp
subst [] x = x
subst ren e = case e of
    Con n c vs -> Con n c $ map f vs
    App n x ys -> App n (f x) (map f ys)
    Lam n x y -> Lam n x (g [x] y)
    Case n x y -> Case n (f x) [(a, g (vars a) b) | (a,b) <- y]
    Let n x y -> Let n [(a,g (map fst x) b) | (a,b) <- x] $ if y `elem` map fst x then y else f y
    Box x -> g [] x
    where
        f x = fromMaybe x $ lookup x ren
        g del x = subst (filter (flip notElem del . fst) ren) x


---------------------------------------------------------------------
-- NAMES

data Name = Name Var Int Int deriving (Data,Typeable,Eq,Ord)

noname = Name "<no name>" 0 0

instance Show Name where
    show x | x == noname = "_"
    show (Name x y z) = "<"++x++","++show y++","++show z++">"

setNameNumber (Name a b _) c = Name a b c

-- pretty print with names for the interesting bits
prettyNames :: Exp -> String
prettyNames x = prettyPrint $ Lambda sl (map (PVar . Ident) free) $ H.Let (BDecls $ map f bind) (H.Var $ UnQual $ Ident root)
    where
        FlatExp free bind root = toFlat x
        f (v,x) = PatBind sl (PVar $ Ident v) Nothing (UnGuardedRhs $ H.App (Lit (String $ show $ getName x)) (toExp x)) (BDecls [])


getName :: Exp -> Name
getName (Con n _ xs) = setNameNumber n $ length xs
getName (App n _ xs) = setNameNumber n $ length xs
getName (Lam n _ _) = n
getName (Case n _ _) = n
getName (Let n _ _) = n
getName (Box x) = getName x

setName :: Exp -> Name -> Exp
setName (Con _ x y) n = Con n x y
setName (App _ x y) n = App n x y
setName (Lam _ x y) n = Lam n x y
setName (Case _ x y) n = Case n x y
setName (Let _ x y) n = Let n x y


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
fromHSE (Module _ _ _ _ _ _ xs) = assignArities [(f, assignNames f x) | (f,x) <- concatMap fromDecl xs]


fromDecl :: Decl -> [(Var,Exp)]
fromDecl (PatBind _ (PVar f) Nothing (UnGuardedRhs x) (BDecls [])) = [(fromName f, fromExp x)]
fromDecl (FunBind [Match _ f vars Nothing (UnGuardedRhs x) (BDecls [])]) = [(fromName f, fromExp $ Lambda sl vars x)]
fromDecl TypeSig{} = []
fromDecl DataDecl{} = []
fromDecl TypeDecl{} = []
fromDecl x = error $ "Unhandled fromDecl: " ++ show x


fromExp :: H.Exp -> Exp
fromExp (Lambda _ [] x) = fromExp x
fromExp (Lambda _ (PVar (Ident x):vars) bod) = Lam noname x $ fromExp $ Lambda sl vars bod
fromExp o@(H.App x y) = Let noname [(f1,fromExp x),(f2,fromExp y),(f3,App noname f1 [f2])] f3
    where f1:f2:f3:_ = freshNames o
fromExp (H.Var (UnQual x)) = App noname (fromName x) []
fromExp (H.Con (UnQual x)) = Con noname (fromName x) []
fromExp (H.Con (Special Cons)) = Con noname ":" []
fromExp (LeftSection x (QVarOp y)) = fromExp $ H.App (H.Var y) x
fromExp (Paren x) = fromExp x
fromExp o@(H.Case x xs) = Let noname [(f1,fromExp x),(f2,Case noname f1 $ map fromAlt xs)] f2
    where f1:f2:_ = freshNames o
fromExp (List []) = Con noname "[]" []
fromExp (List [x]) = fromExp $ InfixApp x (QConOp (Special Cons)) $ List []
fromExp o@(InfixApp x (QConOp (Special Cons)) y) = Let noname [(f1,fromExp x),(f2,fromExp y),(f3,Con noname ":" [f1,f2])] f3
    where f1:f2:f3:_ = freshNames o
fromExp o@(InfixApp a (QVarOp b) c) = fromExp $ H.App (H.App (H.Var b) a) c
fromExp (Lit x) = Con noname (prettyPrint x) []
fromExp x@(NegApp _) = Con noname (prettyPrint x) []
fromExp (If a b c) = fromExp $ H.Case a [f "True" b, f "False" c]
    where f con x = Alt sl (PApp (UnQual $ Ident con) []) (UnGuardedAlt x) (BDecls [])
fromExp o@(H.Let (BDecls xs) x) = Let noname ((f1,fromExp x):concatMap fromDecl xs) f1
    where f1:_ = freshNames o
fromExp o@(Tuple xs) = Let noname
    ((f1, Con noname (fromTuple xs) (take (length xs) fs)) : zipWith (\f x -> (f,fromExp x)) fs xs) f1
    where f1:fs = freshNames o
fromExp (H.Con (Special (TupleCon _ n))) = Con noname (fromTuple $ replicate n ()) []
fromExp x = error $ "Unhandled fromExp: " ++ show x


fromName :: H.Name -> String
fromName (Ident x) = x
fromName (Symbol x) = x

fromAlt :: Alt -> (Pat, Exp)
fromAlt (Alt _ pat (UnGuardedAlt bod) (BDecls [])) = (fromPat pat, fromExp bod)
fromAlt x = error $ "Unhandled fromAlt: " ++ show x

fromPat :: H.Pat -> Pat
fromPat (PParen x) = fromPat x
fromPat (PList []) = Con noname "[]" []
fromPat (PApp (Special Cons) xs) = Con noname ":" $ map fromPatVar xs
fromPat (PInfixApp a b c) = fromPat $ PApp b [a,c]
fromPat (PApp (UnQual c) xs) = Con noname (fromName c) $ map fromPatVar xs
fromPat (PTuple xs) = Con noname (fromTuple xs) $ map fromPatVar xs
fromPat (PApp (Special (TupleCon _ n)) xs) = Con noname (fromTuple xs) $ map fromPatVar xs
fromPat PWildCard = App noname "_wild" []
fromPat x = error $ "Unhandled fromPat: " ++ show x

fromTuple xs = "(" ++ replicate (length xs - 1) ',' ++ ")"

fromPatVar :: H.Pat -> String
fromPatVar (PVar x) = fromName x
fromPatVar x = error $ "Unhandled fromPatVar: " ++ show x


freshNames :: H.Exp -> [String]
freshNames x  = ['v':show i | i <- [1..]] \\ [y | Ident y <- universeBi x]


-- Fixup: Assign names properly
assignNames :: Var -> Exp -> Exp
assignNames fun x = evalState (transformM f x) 0
    where
        f x = do
            modify (+1)
            i <- get
            return $ setName x $ Name fun i 0


-- Fixup: Move arity information
assignArities :: [(Var,Exp)] -> [(Var,Exp)]
assignArities xs = checkPrims $ ("root",App noname (fromJust $ lookup "root" ren) []) :
                                [(fromJust $ lookup a ren, subst ren b) | (a,b) <- xs]
    where ren = [(a, a ++ "'" ++ show (f b)) | (a,b) <- xs]
          f (Lam _ _ x) = 1 + f x
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
toExp (Lam _ x y) = Paren $ lambda [PVar $ Ident x] $ toExp y
toExp (Let _ xs y) = Paren $ H.Let (BDecls $ map toDecl xs) $ toVar y
toExp (App _ x y) = Paren $ foldl H.App (toVar x) $ map toVar y
toExp (Case _ x y) = Paren $ H.Case (toVar x) (map toAlt y)
toExp (Con _ c vs) = Paren $ foldl H.App (H.Con $ UnQual $ toName c) (map toVar vs)
toExp (Box x) = BracketExp $ ExpBracket $ toExp x
toExp x = error $ "toExp, todo: " ++ show x

toAlt :: (Pat, Exp) -> Alt
toAlt (x,y) = Alt sl (toPat x) (UnGuardedAlt $ toExp y) (BDecls [])

toPat :: Pat -> H.Pat
toPat (Con _ c vs) = PApp (UnQual $ toName c) (map (PVar . Ident) vs)
toPat (App _ v []) = PWildCard
toPat x = error $ "toPat, todo: " ++ show x

toVar :: Var -> H.Exp
toVar x = H.Var $ UnQual $ toName x

toName :: String -> H.Name
toName xs@(x:_) | isAlphaNum x || x `elem` "'_" = Ident xs
                | otherwise = Symbol xs

sl = SrcLoc "" 0 0


lambda v1 (Lambda _ v2 x) = Lambda sl (v1++v2) x
lambda v1 (Paren x) = lambda v1 x
lambda v1 x = Lambda sl v1 x

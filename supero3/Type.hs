
module Type(
    Var, Con, Exp(..), lams, pretty,
    Name, noname, incName,
    Env(..), env,
    fromHSE, toHSE
    ) where


import Data.Maybe
import Data.List
import Language.Haskell.Exts hiding (Exp,Name,Pat,Var,Let,App,name)
import qualified Language.Haskell.Exts as H
import Data.Generics.Uniplate.Data


type Var = String
type Con = String

data Name = Name Var Int Int deriving Show

noname = Name "<no name>" 0 0

incName (Name a b c) = Name a b (c+1)


data Exp = Var  Name Var
         | Con  Name Con [Var]
         | App  Name Var Var
         | Lam  Name Var Exp
         | Case Name Var [(Pat,Exp)]
         | Let  Name [(Var,Exp)] Var
         | Box  Exp -- to represent <? ?> brackets
           deriving Show

instance Eq Exp where
    x == y = toExp x == toExp y -- ignore names

pretty :: Exp -> String
pretty = prettyPrint . toExp


type Pat = Exp

lams [] x = x
lams (l:ls) x = Lam noname l $ lams ls x


type Env = Var -> Maybe Exp



env :: [(Var,Exp)] -> Env
env xs x = lookup x xs


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
fromExp x = error $ "Unhandled fromExp: " ++ show x



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
toExp (Lam _ x y) = lambda [PVar $ Ident x] $ toExp y
toExp (Let _ xs y) = H.Let (BDecls $ map toDecl xs) $ toVar y
toExp (App _ x y) = H.App (toVar x) (toVar y)
toExp x = error $ "toExp, todo: " ++ show x

toVar :: Var -> H.Exp
toVar x = H.Var $ UnQual $ Ident x

sl = SrcLoc "" 0 0


lambda v1 (Lambda _ v2 x) = Lambda sl (v1++v2) x
lambda v1 x = Lambda sl v1 x

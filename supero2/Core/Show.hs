
module Core.Show(writeCore) where

import Language.Haskell.Exts as H
import Core.Type as C
import Compiler.Expr
import Data.Generics.PlateData


writeCore :: FilePath -> Prog Core -> IO ()
writeCore file = writeFile file . prettyPrint . prog


prog :: Prog Core -> Module
prog xs = transformBi simp $ Module d d d d d d (map f xs)
    where
        f (name,x) = FunBind [Match d (Ident name) d d (UnGuardedRhs $ expr x) d]

        simp (Match a name b c (UnGuardedRhs (Paren x)) d) = simp $ Match a name b c (UnGuardedRhs x) d
        simp (Match a name vars c (UnGuardedRhs (Lambda _ v x)) d) = simp $ Match a name (vars++v) c (UnGuardedRhs x) d
        simp x = x


expr :: Core -> Exp
expr = transform simp . f
    where
        f (C.App x y) = Paren $ H.App (f x) (f y)
        f (C.Let v x y) = Paren $ H.Let (BDecls [PatBind d (PVar $ Ident v) d (UnGuardedRhs (f x)) d]) (f y)
        f (C.Lam x y) = Paren $ H.Lambda d [PVar $ Ident x] (f y)
        f (C.Var x) = H.Var $ UnQual $ Ident x
        f (C.Fun x) = f $ C.Var x
        f (C.Case x y) = Paren $ H.Case (f x) [Alt d (g a b) (UnGuardedAlt (f c)) d | ((a,b),c) <- y]
        f (C.Con x) = H.Con $ UnQual $ Ident x

        g x y = PApp (UnQual $ Ident x) (map (PVar . Ident) y)

        simp (H.Lambda _ v (H.Lambda _ w y)) = H.Lambda d (v++w) y
        simp (H.App (Paren (H.App x y)) z) = simp $ H.App (H.App x y) z
        simp (H.Lambda _ v (Paren x)) = simp $ H.Lambda d v x
        simp x = x


class Dull a where d :: a
instance Dull [a] where d = []
instance Dull (Maybe a) where d = Nothing
instance Dull Binds where d = BDecls []
instance Dull SrcLoc where d = undefined
instance Dull ModuleName where d = ModuleName d


instance Show Core where
    show = prettyPrint . expr

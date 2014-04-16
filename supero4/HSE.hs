{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

module HSE(deflate, inflate, sl) where

import Data.Data
import Data.List
import Language.Haskell.Exts
import Data.Generics.Uniplate.Data

sl = SrcLoc "" 0 0

---------------------------------------------------------------------
-- DEFLATE

deflate :: Data a => a -> a
deflate = transformBi deflateExp . transformBi deflatePat . transformBi deflateQName . transformBi deflateDecl

spec :: SpecialCon -> QName
spec UnitCon = UnQual $ Ident "()"
spec ListCon = UnQual $ Ident "[]" 
spec Cons = UnQual $ Symbol ":"
spec (TupleCon Boxed i) = UnQual $ Ident $ "(" ++ replicate (i-1) ',' ++ ")"
spec x = Special x

deflateDecl :: Decl -> Decl
deflateDecl (FunBind [Match sl f vars Nothing (UnGuardedRhs x) (BDecls [])]) =
    PatBind sl (PVar f) Nothing (UnGuardedRhs $ Lambda sl vars x) (BDecls [])
deflateDecl x = x

deflateQName :: QName -> QName
deflateQName (Special x) = spec x
deflateQName x = x

deflateExp :: Exp -> Exp
deflateExp (Lambda sl ps x) = foldr (\p x -> Lambda sl [p] x) x ps
deflateExp (LeftSection x (QVarOp y)) = App (Var y) x
deflateExp (List []) = Con $ spec ListCon
deflateExp (List [x]) = Con (spec Cons) `App` x `App` Con (spec ListCon)
deflateExp (Tuple b xs) = foldr App (Con $ spec $ TupleCon b $ length xs) xs
deflateExp (InfixApp a (QVarOp b) c) = Var b `App` a `App` c
deflateExp (InfixApp a (QConOp b) c) = Con b `App` a `App` c
deflateExp (Lit x) = Con $ UnQual $ Ident $ prettyPrint x
deflateExp (If a b c) = Case a [f "True" b, f "False" c]
    where f con x = Alt sl (PApp (UnQual $ Ident con) []) (UnGuardedAlt x) (BDecls [])
deflateExp (Let (BDecls bs) x) = foldr (\b x -> Let (BDecls [b]) x) x bs -- FIXME: Only safe if variables are not mutually recursive
deflateExp x = x

deflatePat :: Pat -> Pat
deflatePat (PInfixApp a b c) = PApp b [a,c]
deflatePat (PList []) = PApp (spec ListCon) []
deflatePat (PTuple b xs) = PApp (spec $ TupleCon b $ length xs) xs
deflatePat x = x


---------------------------------------------------------------------
-- INFLATE

inflate :: Data a => a -> a
inflate =
    transformBi inflateRhs . transformBi inflateAlt . transformBi inflateGuardedAlts .
    transformBi inflatePat . transformBi inflateExp .
    transformBi Paren . transformBi PParen

inflateExp :: Exp -> Exp
inflateExp (Lambda sl ps (Paren x)) = inflateExp $ Lambda sl ps x
inflateExp (Lambda sl ps1 (Lambda _ ps2 x)) | null $ names ps1 `intersect` names ps2 = Lambda sl (ps1++ps2) x
inflateExp (Paren (Paren x)) = Paren x
inflateExp (Paren (Var x)) = Var x
inflateExp (Paren (Con x)) = Con x
inflateExp (App (Paren (App a b)) c) = App (App a b) c
inflateExp (Con (UnQual (Symbol "[]"))) = List []
inflateExp x = x

inflatePat :: Pat -> Pat
inflatePat (PParen (PParen x)) = PParen x
inflatePat (PParen (PVar x)) = PVar x
inflatePat (PApp (UnQual (Symbol "[]")) []) = PList []
inflatePat x = x

inflateRhs :: Rhs -> Rhs
inflateRhs (UnGuardedRhs (Paren x)) = UnGuardedRhs x
inflateRhs x = x

inflateAlt :: Alt -> Alt
inflateAlt (Alt sl (PParen p) x y) = Alt sl p x y
inflateAlt x = x

inflateGuardedAlts :: GuardedAlts -> GuardedAlts
inflateGuardedAlts (UnGuardedAlt (Paren x)) = UnGuardedAlt x
inflateGuardedAlts x = x

names :: Data a => a -> [String]
names = map f . universeBi
    where f (Ident x) = x
          f (Symbol x) = x

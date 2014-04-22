{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

module HSE(deflate, inflate, noCAF, sl) where

import Data.Data
import Data.List
import Language.Haskell.Exts
import Data.Generics.Uniplate.Data
import Control.Applicative

sl = SrcLoc "" 0 0

names :: Data a => a -> [String]
names = map f . universeBi
    where f (Ident x) = x
          f (Symbol x) = x

fresh :: [String] -> [String]
fresh del = ["v" ++ show i | i  <- [1..]] \\ del

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
deflateExp (List (x:xs)) = Paren $ Con (spec Cons) `App` Paren x `App` deflateExp (List xs)
deflateExp (Tuple b xs) = foldl App (Con $ spec $ TupleCon b $ length xs) xs
deflateExp (InfixApp a (QVarOp b) c) = Var b `App` a `App` c
deflateExp (InfixApp a (QConOp b) c) = Con b `App` a `App` c
deflateExp (Lit x) = Con $ UnQual $ Ident $ prettyPrint x
deflateExp (NegApp x) = Paren $ Var (UnQual $ Ident "negate") `App` Paren x
deflateExp (Case (Var (UnQual v)) (Alt sl (PVar p) (UnGuardedAlt e) (BDecls []):_))
    | v == p = e
    | otherwise = Let (BDecls [PatBind sl (PVar p) Nothing (UnGuardedRhs $ Var $ UnQual v) (BDecls [])]) e
deflateExp (If a b c) = Case a [f "True" b, f "False" c]
    where f con x = Alt sl (PApp (UnQual $ Ident con) []) (UnGuardedAlt x) (BDecls [])
deflateExp (Let (BDecls bs) x) = foldr (\b x -> Let (BDecls [b]) x) x bs -- FIXME: Only safe if variables are not mutually recursive
deflateExp (EnumFromTo x y) = Var (UnQual $ Ident "enumFromTo") `App` x `App` y
deflateExp (EnumFrom x) = Var (UnQual $ Ident "enumFrom") `App` x
deflateExp (ListComp res xs) = lst xs
    where
        -- variants returning a Maybe
        may [] = Just $ Con (UnQual $ Ident "Just") `App` Paren res
        may (QualStmt (LetStmt bind):xs) = deflateExp . Let bind <$> may xs
        may (QualStmt (Qualifier e):xs) = (\xs -> Paren $ deflateExp $ If e xs $ Con $ UnQual $ Ident "Nothing") <$> may xs
        may _ = Nothing

        -- optimised shortcuts (use map or mapMaybe)
        lst (QualStmt (Generator _ (PVar p) e):[]) = Var (UnQual $ Ident "map") `App` deflateExp (Lambda sl [PVar p] res) `App` e
        lst o@(QualStmt (Generator _ p e):xs) | Just ans <- may xs =
            Var (UnQual $ Ident "mapMaybe") `App` deflateExp (Lambda sl [PVar new] $ bod ans) `App` e
            where new:_ = map Ident $ fresh $ names $ ListComp res o
                  bod ans = deflateExp $ Case (Var $ UnQual new)
                            [Alt sl p (UnGuardedAlt ans) $ BDecls []
                            ,Alt sl PWildCard (UnGuardedAlt $ Con $ UnQual $ Ident "Nothing") $ BDecls []]

        -- from the report, returning a list
        lst o@(QualStmt (Generator _ p e):xs) = Var (UnQual $ Ident "concatMap") `App` deflateExp (Lambda sl [PVar new] bod) `App` e
          where new:_ = map Ident $ fresh $ names $ ListComp res o
                bod = deflateExp $ Case (Var $ UnQual new)
                          [Alt sl p (UnGuardedAlt $ lst xs) $ BDecls []
                          ,Alt sl PWildCard (UnGuardedAlt $ deflateExp $ List []) $ BDecls []]
        lst (QualStmt (Qualifier e):xs) = Paren $ deflateExp $ If e (lst xs) (deflateExp $ List [])
        lst (QualStmt (LetStmt bind):xs) = Paren $ deflateExp $ Let bind $ lst xs
        lst [] = deflateExp $ List [res]
        lst xs = ListComp res xs
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
inflateExp (Paren (Paren x)) = inflateExp $ Paren x
inflateExp (Paren (Var x)) = Var x
inflateExp (Paren (Con x)) = Con x
inflateExp (Paren (List x)) = List x
inflateExp (Paren (Lit x)) = Lit x
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


---------------------------------------------------------------------
-- CAF AVOIDANCE

noCAF :: Module -> Module
noCAF (Module a b c d exp e bod) = inflate $ Module a b c d exp e $ transformBi addDefn $ transformBi addCall bod
    where
        bad = [n | PatBind _ (PVar n) _ (UnGuardedRhs e) (BDecls []) <- bod, not $ isZeroCost e] \\ universeBi exp
        addDefn (PatBind sl (PVar n) ty (UnGuardedRhs e) dec) | n `elem` bad =
                (PatBind sl (PVar n) ty (UnGuardedRhs $ Lambda sl [PApp (Special UnboxedSingleCon) []] e) dec)
        addDefn x = x
        addCall (Var (UnQual n)) | n `elem` bad = App (Var $ UnQual n) (Con $ Special UnboxedSingleCon)
        addCall x = x

isZeroCost Lambda{} = True
isZeroCost (Let (BDecls [PatBind _ (PVar _) _ (UnGuardedRhs e1) (BDecls [])]) e2) = isZeroCost e1 && isZeroCost e2
isZeroCost (Var _) = True
isZeroCost (Paren x) = isZeroCost x
isZeroCost _ = False

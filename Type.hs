
module Type where

import Yhc.Core
import qualified Data.Map as Map
import Data.Play
import Data.List
import Text.PrettyPrint.HughesPJ



type FuncMap = Map.Map String Func

data Prog = Prog {funcs :: FuncMap}


data Func = Func {funcName :: String, funcAlts :: [FuncAlt]}


data FuncAlt = FuncAlt {altNum :: Int, altMatch :: [Expr], altBody :: Expr}


data Expr = Var Int
          | Case Expr [(Expr, Expr)]
          | Apply Expr [Expr]
          | Fun String
          | Ctr String
          | Prim String
          | Const Const
          deriving (Eq,Show)


data Const = ConstStr String
           | ConstInt Int
           | ConstInteger Integer
           | ConstChr Char
           deriving Eq


instance Show Const where
    show (ConstStr x) = show x
    show (ConstInt x) = show x ++ "#"
    show (ConstInteger x) = show x ++ "##"
    show (ConstChr x) = show x


isVar (Var{}) = True; isVar _ = False

mkApply x [] = x
mkApply x xs = Apply x xs



instance Play Expr where
    replaceChildren x =
        case x of
            Case x xs -> (x : concatMap (\(a,b) -> [a,b]) xs,
                         \(y:ys) -> Case y (f ys))
                where
                    f [] = []
                    f (a:b:xs) = (a,b) : f xs
            
            Apply x xs -> (x:xs, \(x:xs) -> Apply x xs)
            
            _ -> playDefault x



instance Show Prog where
    show (Prog funcs) = concat $ intersperse "\n\n" $ map show $ Map.elems funcs


instance Show Func where
    show = render . docFunc
    
instance Show FuncAlt where
    show = render . docFuncAlt ""


docFunc :: Func -> Doc
docFunc (Func name xs) = vcat (map (docFuncAlt name) xs)


docFuncAlt :: String -> FuncAlt -> Doc
docFuncAlt name (FuncAlt i conds x) =
    hsep $
        text (name ++ "$" ++ show i) :
        map (docExpr True) conds ++
        text "=" : docExpr False x : []


docExpr :: Bool -> Expr -> Doc
docExpr = f
    where
        f _ (Var i) = text $ show i
        f _ (Ctr x) = text x
        f _ (Fun x) = text x
        f _ (Prim x) = text (x ++ "#")
        f _ (Const x) = text $ show x
        
        f b (Apply x xs) = p b $ f True x <+> hsep (map (f True) xs)

        f b (Case on alts) = p b $
                text "case" <+> f True on <+> text "of" $$ inner (vcat $ map g alts)
            where
                g (a,b) = f False a <+> text "->" <+> f False b

        f _ x = text $ show x
        
        p b = if b then parens else id
        

inner = nest 4

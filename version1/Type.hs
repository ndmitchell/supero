
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
          | FunAlt String Int
          | Ctr String
          | Prim String
          | Const Const
          | Eval Expr
          | Jail Expr
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



onBody_Prog :: (Expr -> Expr) -> (Prog -> Prog)
onBody_Prog f (Prog x) = Prog $ onBody_Funcs f x

onBody_Funcs :: (Expr -> Expr) -> (FuncMap -> FuncMap)
onBody_Funcs f = Map.map (onBody_Func f)

onBody_Func :: (Expr -> Expr) -> (Func -> Func)
onBody_Func f func = func{funcAlts = map (onBody_Alt f) (funcAlts func)}

onBody_Alt :: (Expr -> Expr) -> (FuncAlt -> FuncAlt)
onBody_Alt f alt = alt{altBody = f (altBody alt)}


getFuncAlt :: Func -> Int -> FuncAlt
getFuncAlt func i = head [alt | alt <- funcAlts func, altNum alt == i]


isVar (Var{}) = True; isVar _ = False
isEval (Eval{}) = True; isEval _ = False

fromEval (Eval x) = x
fromEval x = x

remFunAlt (FunAlt x i) = Fun x
remFunAlt x = x

toFunAlt (Fun x) = (FunAlt x 0)
toFunAlt x = x

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
            
            Eval x -> playOne Eval x
            Jail x -> playOne Jail x
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
        f _ (FunAlt x i) = text $ x ++ "$" ++ show i
        f _ (Eval x) = braces $ f False x
        f _ (Jail x) = brackets $ f False x
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

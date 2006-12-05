
module Type where

import Yhc.Core
import qualified Data.Map as Map
import Data.List
import Data.Play
import Data.Maybe
import Control.Monad
import Text.PrettyPrint.HughesPJ

import Debug.Trace


type FuncMap = Map.Map String Func
type Binding = [(Expr, Expr)]

data Prog = Prog {funcs :: FuncMap}


data Func = Func {funcName :: String, funcAlts :: [([Expr], Expr)]}


data Expr = Var Int
          | Lambda [Int] Expr
          | Case Expr [(Expr, Expr)]
          | Apply Expr [Expr]
          | Fun String
          | Ctr String
          | Prim String
          | Const Const
          | Jail Expr
          | Any
          deriving (Eq,Show)


data Const = ConstStr String
           | ConstInt Int
           | ConstChr Char
           deriving Eq


instance Show Const where
    show (ConstStr x) = show x
    show (ConstInt x) = show x ++ "#"
    show (ConstChr x) = show x


isVar (Var{}) = True; isVar _ = False
isJail (Jail{}) = True; isJail _ = False

mkApply x [] = x
mkApply x xs = Apply x xs


-- find the RHS that matches perfectly
findExactRhs :: Func -> [Expr] -> Maybe Expr
findExactRhs func args = liftM (uncurry replaceBinding) $
        listToMaybe $ filter (isValid . fst) $ matchBindings func args
    where
        -- to be a perfect match each variable must occur exactly once
        -- in lhs and rhs
        -- and both sides must be var's
        isValid xs = let (a,b) = unzip xs in f a && f b
        f x = all isVar x && length x == length (nub x)


{-
NOTE ON ARITIES:

Given:        f x = body
asking for:   f y z
Gives:        Apply (body[x/y]) z
-}

findBestRhs :: Func -> [Expr] -> Maybe Expr
findBestRhs func args = listToMaybe $ concatMap f $ funcAlts func
    where
        f (lhs,rhs) = case matchBinding lhs used of
                          Just bind | isValid bind -> [mkApply (replaceBinding bind rhs) other]
                          Nothing -> []
            where
                (used,other) = splitAt (length lhs) args

        isValid _ = True



matchBindings :: Func -> [Expr] -> [(Binding,Expr)]
matchBindings func call = [(bind,rhs) | (lhs,rhs) <- funcAlts func, Just bind <- [matchBinding lhs call]]


-- lhs may be more general than rhs
matchBinding :: [Expr] -> [Expr] -> Maybe Binding
matchBinding xs ys = liftM nub $ fs xs ys
    where
        fs [] [] = return []
        fs (x:xs) (y:ys) = do
            res <- f x y
            rest <- fs xs ys
            return (res++rest)
        fs _ _ = Nothing
        
        f (Var x) y = Just [(Var x,y)]
        f (Apply x xs) (Apply y ys) = fs (x:xs) (y:ys)
        f x y = if x == y then Just [] else Nothing



replaceBinding :: Binding -> Expr -> Expr
replaceBinding bind x =
    case x of
        Case on alts -> Case (f on) (map g alts)
            where
                g (lhs,rhs) = (lhs, replaceBinding bind2 rhs)
                    where
                        free = [Var x | Var x <- allOver lhs]
                        bind2 = filter ((`notElem` free) . fst) bind
        
        x@(Var _) -> fromMaybe x (lookup x bind)
        x -> generate (map f children)
            where (children,generate) = replaceChildren x
    
    where
        f = replaceBinding bind




instance Play Expr where
    replaceChildren x =
        case x of
            Lambda n x -> playOne (Lambda n) x
            
            Case x xs -> (x : concatMap (\(a,b) -> [a,b]) xs,
                         \(y:ys) -> Case y (f ys))
                where
                    f [] = []
                    f (a:b:xs) = (a,b) : f xs
            
            Apply x xs -> (x:xs, \(x:xs) -> Apply x xs)
            Jail x -> playOne Jail x
            
            _ -> playDefault x



instance Show Prog where
    show (Prog funcs) = concat $ intersperse "\n\n" $ map show $ Map.elems funcs


instance Show Func where
    show = render . docFunc


docFunc :: Func -> Doc
docFunc (Func name xs) = vcat (map f xs)
    where
        f (conds,x) = text name <+> hsep (map (docExpr True) conds) <+> text "=" <+> docExpr False x


docExpr :: Bool -> Expr -> Doc
docExpr = f
    where
        f _ (Var i) = text $ show i
        f _ (Ctr x) = text x
        f _ (Fun x) = text x
        f _ (Prim x) = text (x ++ "#")
        f _ (Jail x) = braces $ f False x
        f _ (Const x) = text $ show x
        
        f b (Apply x xs) = p b $ f True x <+> hsep (map (f True) xs)

        f b (Case on alts) = p b $
                text "case" <+> f True on <+> text "of" $$ inner (vcat $ map g alts)
            where
                g (a,b) = f False a <+> text "->" <+> f False b

        f _ x = text $ show x
        
        p b = if b then parens else id
        

inner = nest 4

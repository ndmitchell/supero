
module Match(Binding, findExactRhs, findBestRhs, matchBinding, replaceBinding) where

import Type
import Data.List
import Data.Play
import Data.Maybe
import Control.Monad
import General


type Binding = [(Int, Expr)]



-- find the RHS that matches perfectly
findExactRhs :: Func -> [Expr] -> Maybe (Int,Binding,Expr)
findExactRhs func call = listToMaybe
    [(n,bind,replaceBinding bind rhs)
        | FuncAlt n lhs rhs <- funcAlts func, Just bind <- [matchBinding lhs call], isValid bind]
    where
        -- to be a perfect match each RHS must be a unique Var
        isValid xs = all isVar b && unique b
            where b = map snd xs



{-
NOTE ON ARITIES:

Given:        f x = body
asking for:   f y z
Gives:        Apply (body[x/y]) z
-}

findBestRhs :: Func -> [Expr] -> Maybe (Int,Binding,Expr)
findBestRhs func args = listToMaybe $ concatMap f $ funcAlts func
    where
        f (FuncAlt n lhs rhs) =
            case matchBinding lhs used of
                Just bind -> [(n, bind, mkApply (replaceBinding bind rhs) other)]
                Nothing -> []
            where
                (used,other) = splitAt (length lhs) args



-- | Given two expressions, give a substitution
--   of free variables in the LHS to items, which when
--   substituted gives the RHS
--
--   matchBinding LHS RHS = Just binding
--   iff LHS[binding] = RHS
matchBinding :: [Expr] -> [Expr] -> Maybe Binding
matchBinding xs ys = fs xs ys >>= check
    where
        fs [] [] = return []
        fs (x:xs) (y:ys) = do
            res <- f x y
            rest <- fs xs ys
            return (res++rest)
        fs _ _ = Nothing
        
        f (Var x) y = Just [(x,y)]
        f (Apply x xs) (Apply y ys) = fs (x:xs) (y:ys)
        f x y = if x == y then Just [] else Nothing
        
        
        check bind = if unique (map fst bind2) then Just bind2 else Nothing
            where bind2 = nub bind
        



-- | Given a binding, where every lhs of a Variable,
--   replace the appropriate places in the expression
replaceBinding :: Binding -> Expr -> Expr
replaceBinding bind x =
    case x of
        Case on alts -> Case (f on) (map g alts)
            where
                g (lhs,rhs) = (lhs, replaceBinding bind2 rhs)
                    where
                        free = [x | Var x <- allOver lhs]
                        bind2 = filter ((`notElem` free) . fst) bind
        
        (Var x) -> fromMaybe (Var x) (lookup x bind)
        x -> generate (map f children)
            where (children,generate) = replaceChildren x
    
    where
        f = replaceBinding bind



module Match(Binding, findExactRhs, findBestRhs, matchBinding, replaceBinding) where

import Type
import Data.List
import Data.Play
import Data.Maybe
import Control.Monad


type Binding = [(Expr, Expr)]



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
        f (FuncAlt _ lhs rhs) =
            case matchBinding lhs used of
                Just bind | isValid bind -> [mkApply (replaceBinding bind rhs) other]
                Nothing -> []
            where
                (used,other) = splitAt (length lhs) args

        isValid _ = True



matchBindings :: Func -> [Expr] -> [(Binding,Expr)]
matchBindings func call = [(bind,rhs) | FuncAlt _ lhs rhs <- funcAlts func, Just bind <- [matchBinding lhs call]]


-- | Given two expressions, give a substitution
--   of free variables in the LHS to items, which when
--   substituted gives the RHS
--
--   matchBinding LHS RHS = Just binding
--   iff LHS[binding] = RHS
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



-- | Given a binding, where every lhs of a Variable,
--   replace the appropriate places in the expression
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


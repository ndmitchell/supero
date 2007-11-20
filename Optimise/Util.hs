
module Optimise.Util where

import Yhc.Core


unwrapLet (CoreLet x y) = (CoreLet x,y)
unwrapLet x = (id,x)

unwrapCase (CoreCase x y) = (flip CoreCase y,x)
unwrapCase x = (id,x)

unwrapApp (CoreApp x y) = (flip CoreApp y,x)
unwrapApp x = (id,x)


inlineLetBind (CoreLit{}) = True
inlineLetBind (CoreLam{}) = True
inlineLetBind _ = False


fromCoreLetDeep (CoreLet x y) = (x++a,b)
    where (a,b) = fromCoreLetDeep y
fromCoreLetDeep x = ([],x)

exprSize :: CoreExpr -> Int
exprSize = length . universe

exprSizeOld :: CoreExpr -> Int
exprSizeOld = para (\_ cs -> 1 + maximum (0:cs))

comparing x = on compare x

on f g x y = f (g x) (g y)

fixM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
fixM f x = do
    x2 <- f x
    if x == x2 then return x2 else fixM f x2

-- need to blur all uses and definitions
blurVar = transform f
    where
        f (CoreVar _) = CoreVar ""
        f (CoreLet bind x) = CoreLet (map ((,) "" . snd) bind) x
        f (CoreCase on alts) = CoreCase on [(g a,b) | (a,b) <- alts]
        f (CoreLam x y) = CoreLam (map (const "") x) y
        f x = x

        g (PatCon x _) = PatCon x []
        g x = x


blurLit = transform f
    where
        f (CoreLit _) = CoreLit (CoreInt 0)
        f (CoreCase on alts) = CoreCase on [(g a,b) | (a,b) <- alts]
        f x = x

        g (PatLit _) = PatLit (CoreInt 0)
        g x = x


splits :: [a] -> [([a],a,[a])]
splits [] = []
splits (x:xs) = ([],x,xs) : [(x:a,b,c) | (a,b,c) <- splits xs]


lookupRev :: Eq b => b -> [(a,b)] -> Maybe a
lookupRev x ((a,b):xs) | x == b = Just a
                       | otherwise = Nothing
lookupRev _ _ = Nothing


disjoint xs ys = all (`notElem` xs) ys


(~~) :: Show a => String -> a -> String
(~~) lhs rhs = lhs ++ "\n" ++ show rhs


module Firstify.Template(
    isHO, isLambda, isConLambda, lamArity,
    Template, genTemplate, useTemplate
    ) where

import Yhc.Core hiding (collectAllVars,collectFreeVars,uniqueBoundVars,replaceFreeVars)
import Yhc.Core.FreeVar2

import Data.List
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

{-
SPECIALISE ALGORITHM

Need to generate a specialised version if:
* f gets called with more arguments than its arity
* any argument is higher order

The specialised version has:
* a free variable for each non-ho argument
* the free variables within a function, for a ho argument
-}

isHO :: CoreExpr -> Bool
isHO x = isLambda x || isConLambda x


isLambda :: CoreExpr -> Bool
isLambda (CoreLet _ x) = isLambda x
isLambda (CoreLam _ _) = True
isLambda (CoreCase x ys) = any (isLambda . snd) ys
isLambda _ = False


isConLambda :: CoreExpr -> Bool
isConLambda (CoreLet _ x) = isConLambda x
isConLambda (CoreCase x ys) = any (isConLambda . snd) ys
isConLambda (CoreApp (CoreCon _) args) = any isLambda args
isConLambda _ = False


lamArity :: CoreExpr -> Int
lamArity (CoreLet _ x) = lamArity x
lamArity (CoreLam xs x) = length xs + lamArity x
lamArity (CoreCase x ys) = maximum $ map (lamArity . snd) ys
lamArity _ = 0


data Template = Template [CoreExpr]
                deriving (Eq,Ord,Show)


-- given a call to this function, with the given arguments
-- return Just (Template, [Args]) if you want to make it a new call
useTemplate :: CoreFunc -> [CoreExpr] -> Maybe (Template, [CoreExpr])
useTemplate func xs | nxs >= ar && (nxs > ar || any isHO xs)
        = Just (Template ts, concat xs2)
    where
        nxs = length xs
        ar = length $ coreFuncArgs func
        
        (ts,xs2) = unzip $ map f xs

        f x | isHO x    = (normVars x  , map CoreVar $ collectFreeVars x)
            | otherwise = (CoreVar "v1", [x])

useTemplate _ _ = Nothing



genTemplate :: Template -> CoreFunc -> CoreFuncName -> CoreFunc
genTemplate (Template xs) (CoreFunc _ args body) newname = CoreFunc newname (concat args2) body2
    where
        (norm,extra) = splitAt (length args) reps
        (args2,reps) = runFreeVars $ do deleteVars (concatMap collectAllVars (body:xs))
                                        mapAndUnzipM f xs
        
        body2 = coreApp (replaceFreeVars (zip args norm) body) extra

        -- return the arguments you require, and the expression you are    
        f :: CoreExpr -> FreeVar ([CoreVarName], CoreExpr)
        f x = do
            let free = collectFreeVars x
            vs <- replicateM (length $ collectFreeVars x) getVar
            return (vs, replaceFreeVars (zip free $ map CoreVar vs) x)



-- for all equivalent expressions
-- irrelevant of free or bound var names, alpha rename to the same thing


data Stat = Stat {statFree :: Int, statBound :: Int, seenFree :: Map.Map String String}

normVars :: CoreExpr -> CoreExpr
normVars x = evalState (f Map.empty x) (Stat 1 1 Map.empty)
    where
        getF x = do s <- get
                    case Map.lookup x (seenFree s)  of
                        Nothing -> do
                            let new = 'f' : show (statFree s)
                            put s{statFree = statFree s + 1
                                 ,seenFree = Map.insert x new (seenFree s)}
                            return new
                        Just y -> return y

        getB = do s <- get
                  put s{statBound = statBound s + 1}
                  return $ 'b':show (statBound s)

        f mp (CoreVar x) =
            case Map.lookup x mp of
                Nothing -> liftM CoreVar (getF x)
                Just y -> return $ CoreVar y
        
        f mp (CoreCase on alts) = do
                on2 <- f mp on
                alts2 <- mapM g alts
                return $ CoreCase on2 alts2
            where
                g (CoreVar lhs,rhs) = do
                    l <- getB
                    r <- f (Map.insert lhs l mp) rhs
                    return (CoreVar l, r)
                
                g (CoreApp c lhs,rhs) = do
                    l <- replicateM (length lhs) getB
                    r <- f (Map.union (Map.fromList $ zip (map fromCoreVar lhs) l) mp) rhs
                    return (CoreApp c (map CoreVar l), r)

        f mp (CoreLet bind xs) = do
                bs <- replicateM (length bind) getB
                xs2 <- f (Map.union (Map.fromList $ zip (map fst bind) bs) mp) xs
                return $ CoreLet (zip bs $ map snd bind) xs2

        f mp (CoreLam x xs) = do
                bs <- replicateM (length x) getB
                xs2 <- f (Map.union (Map.fromList $ zip x bs) mp) xs
                return $ CoreLam bs xs2
        
        f mp x = do
            xs2 <- mapM (f mp) (getChildrenCore x)
            return $ setChildrenCore x xs2

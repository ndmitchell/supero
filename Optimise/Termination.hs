
module Optimise.Termination where

import Optimise.State
import Yhc.Core
import Yhc.Core.UniqueId
import Yhc.Core.FreeVar3
import Control.Monad
import Data.List
import Optimise.Util


termination :: [(String,Termination)]
termination =
    [("none",none)
    ,("jonssonish",jonssonish)
    ]


none c = return $ Just $ current c



---------------------------------------------------------------------
-- FROM THE BEFORE TIME



jonssonish :: Context -> SS (Maybe CoreExpr)
jonssonish context =
    if null whistle then return Nothing else do
        (t,subs) <- msg (head whistle) x
        sioPrint $ "\n\nonf whistle" ~~ x ~~ head whistle ~~ t ~~ subs
        let binds = [(v,e) | (v,(_,e)) <- subs]
            freeBinds = nub $ concatMap (collectFreeVars . snd) binds
            freeNorm = collectFreeVars x

        if True || isCoreVar t || not (null (freeBinds \\ freeNorm)) then
            return $ Just x
         else
            return $ Just $ coreLet binds t
    where
        x = current context
        whistle = filter (<<| x) (rhoCurrent context)


type Subst = [(CoreVarName,CoreExpr)]
type SubstPair = [(CoreVarName,(CoreExpr,CoreExpr))]

-- | Most specific generalisation
--
--   Taken from "An Algorithm of Generalization in Positive Supercompilation"
--   by Sorensen and Gluck, Algorithm 4.6
msg :: UniqueIdM m => CoreExpr -> CoreExpr -> m (CoreExpr, SubstPair)
msg x y = do v <- getVar ; f (CoreVar v) [(v,(x,y))]
    where
        -- rule 1
        f :: UniqueIdM m => CoreExpr -> SubstPair -> m (CoreExpr, SubstPair)
        f expr bind | not $ null match = do
                let xc = children x
                    yc = children y
                vs <- replicateM (length xc) getVar
                let expr2 = replaceFreeVars [(v, snd (uniplate x) (map CoreVar vs))] expr
                    bind2 = zip vs (zip xc yc) ++ miss ++ rest
                f expr2 bind2
            where
                (match,miss) = partition (\(_,(x,y)) -> x `eq1` y) bind
                (v,(x,y)):rest = match

        -- rule 2
        f expr bind | not $ null match = f
                  (replaceFreeVars [(x,CoreVar y)] expr)
                  (filter ((/=) y . fst) bind)
            where
                match = [(x,y) | (x,(s1,t1)) <- bind, (y,(s2,t2)) <- bind, x /= y, s1 == s2, t1 == t2]
                (x,y):_ = match

        f expr bind = return (expr,bind)


-- | Homeomorphic embedding
(<<|) :: CoreExpr -> CoreExpr -> Bool
CoreLit _ <<| CoreLit _ = True -- can omit as I don't do maths?
CoreVar _ <<| CoreVar _ = True
x <<| y =
    any (x <<|) (children y) ||
    (x `eq1` y && and (zipWith (<<|) (children x) (children y)))



-- | Least common anti-instance
lca :: UniqueIdM m => CoreExpr -> CoreExpr -> m (CoreExpr, Subst, Subst)
lca x y | x `eq1` y = do
        let rep = snd $ uniplate x
        (cs,sx,sy) <- liftM unzip3 $ zipWithM lca (children x) (children y)
        return (rep cs, concat sx, concat sy)
lca x y = do
    v <- getVar
    return (CoreVar v, [(v,x)], [(v,y)])

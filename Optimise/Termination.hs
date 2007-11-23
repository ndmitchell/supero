
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
    ,("always",always)
    ,("whistle",whistle)
    ,("hash1",hash 1)
    ,("hash2",hash 2)
    ,("hash3",hash 3)
    ,("hash4",hash 4)
    ,("hash5",hash 5)
    ,("hash6",hash 6)
    ,("hash7",hash 7)
    ,("size1",size 1)
    ,("size2",size 2)
    ,("size3",size 3)
    ,("size4",size 4)
    ,("size5",size 5)
    ,("size6",size 6)
    ,("size7",size 7)
    ,("size8",size 8)
    ,("size9",size 9)
    ,("size10",size 10)
    ,("size11",size 11)
    ,("size12",size 12)
    ,("size13",size 13)
    ,("jonish",jonish)
    ]


none c = return $ Just $ current c

always _ = return Nothing


whistle :: Context -> SS (Maybe CoreExpr)
whistle Context{rho=rho, current=x} = return $
    if null $ filter (<<| x) rho
    then Nothing
    else Just x

hash :: Int -> Context -> SS (Maybe CoreExpr)
hash depth Context{rho=rho, current=x} = return $
        if any ((==) x . root) rho
        then Nothing
        else Just x
    where
        root = f depth
        rootx = root x

        f 0 _ = CoreVar ""
        f _ (CoreVar _) = CoreVar ""
        f n x = descend (f (n-1)) x

size :: Int -> Context -> SS (Maybe CoreExpr)
size depth Context{current=x} = return $
        if exprSizeOld x > depth then Nothing else Just x

---------------------------------------------------------------------
-- FROM THE BEFORE TIME



jonish :: Context -> SS (Maybe CoreExpr)
jonish context =
    if null whistle then return Nothing else do
        (t,subs) <- msg (head whistle) x
        -- sioPrint $ "\n\nonf whistle" ~~ x ~~ head whistle ~~ t ~~ subs
        let binds = [(v,e) | (v,(_,e)) <- subs]
            freeBinds = nub $ concatMap (collectFreeVars . snd) binds
            freeNorm = collectFreeVars x

        if True || isCoreVar t || not (null (freeBinds \\ freeNorm)) then
            return $ Just x
         else
            return $ Just $ coreLet binds t
    where
        x = current context
        whistle = filter (<<| x) (rho context)


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
                (match,miss) = partition (\(_,(x,y)) -> x `eq1CoreExpr` y) bind
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
(<<|) x y = if null (children x) then False else
            f (blurVar $ blurLit x) (blurVar $ blurLit y)
    where
        f x y = any (x <<|) ys ||
                (x `eq1CoreExpr` y && length xs == length ys && and (zipWith (<<|) xs ys))
            where
                xs = children x
                ys = children y


-- | Least common anti-instance
lca :: UniqueIdM m => CoreExpr -> CoreExpr -> m (CoreExpr, Subst, Subst)
lca x y | x `eq1CoreExpr` y = do
        let rep = snd $ uniplate x
        (cs,sx,sy) <- liftM unzip3 $ zipWithM lca (children x) (children y)
        return (rep cs, concat sx, concat sy)
lca x y = do
    v <- getVar
    return (CoreVar v, [(v,x)], [(v,y)])

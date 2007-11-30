
module Optimise.Termination where

import Optimise.State
import Yhc.Core
import Yhc.Core.UniqueId
import Yhc.Core.FreeVar3
import Control.Monad
import Data.List
import Data.Maybe
import Safe
import Optimise.Util


termination :: [(String,Termination)]
termination =
    [("none",none)
    ,("always",always)
    ,("whistle",whistle)
    ,("msg",embedMsg)
    ,("general",general)
    ]


none c = return $ Just $ current c

always _ = return Nothing


embedMsg :: Context -> SS (Maybe CoreExpr)
embedMsg Context{rho=rho, current=x} =
    let bad = filter (<<| x) rho in
    if null bad then return Nothing
    else if head bad `eqAlphaCoreExpr` x then
        ( {- if head bad `elem` currents then
            return $ Just $ CoreFun "non_termination"
        else -}
            return Nothing
        )
    else do
        new <- head bad `msgGluck` x
        {-
        sioPutStrLn ""
        sioPutStrLn $ show $ head bad
        sioPutStrLn "<<|"
        sioPutStrLn $ show x
        sioPutStrLn "<<|+"
        sioPutStrLn $ show new
        sioPause
        -}
        return $ Just new

        {-
    
        
        -- must compare against the first in the list
        -- otherwise you keep comparing against yourself
        (_,new) <- msg2 (last bad) x
        sioPutStrLn $ show new
        -- sioPause
        return $ Just new
        
        -}



general :: Context -> SS (Maybe CoreExpr)
general Context{rho=rho, current=x} =
    let bad = filter (<<| x) rho in
    if null bad then return Nothing
    else if head bad `eqAlphaCoreExpr` x then
        ( {- if head bad `elem` currents then
            return $ Just $ CoreFun "non_termination"
        else -}
            return Nothing
        )
    else do
        new <- head bad <<|+ x
        {-
        sioPutStrLn ""
        sioPutStrLn $ show $ head bad
        sioPutStrLn "<<|"
        sioPutStrLn $ show x
        sioPutStrLn "<<|+"
        sioPutStrLn $ show new
        sioPause
        -}
        return $ Just new

        {-
    
        
        -- must compare against the first in the list
        -- otherwise you keep comparing against yourself
        (_,new) <- msg2 (last bad) x
        sioPutStrLn $ show new
        -- sioPause
        return $ Just new
        
        -}

whistle :: Context -> SS (Maybe CoreExpr)
whistle Context{rho=rho, current=x} = return $
    if null $ filter (<<| x) rho
    then Nothing
    else Just x

{-
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
-}

---------------------------------------------------------------------
-- FROM THE BEFORE TIME


{-
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
-}

type Subst = [(CoreVarName,CoreExpr)]
type SubstPair = [(CoreVarName,(CoreExpr,CoreExpr))]



coreLet' bind (CoreVar x) = fromMaybe (coreLet bind (CoreVar x)) $ lookup x bind
coreLet' bind x = coreLet bind x



-- if c(xs) <<| c(ys), then find differences within and float them up
-- if x <<| a child, then descend and lambda float it
-- spit out a modified version of the second, attempting to be more like the first
(<<|+) :: UniqueIdM m => CoreExpr -> CoreExpr -> m CoreExpr
a <<|+ b | any (a <<|) (children b) = do
        v <- getVar
        let (lam,bod) = f v (collectFreeVars b) a b
        lam <- duplicateExpr lam
        return $ coreLet [(v,lam)] bod
    where
        -- the first CoreExpr is probably a CoreLam, with duplicate variables (lam bindings)
        -- the second is the whole removed
        f :: CoreVarName -> [CoreVarName] -> CoreExpr -> CoreExpr -> (CoreExpr, CoreExpr) 
        f v vars a b = case filter ((a <<|) . fst) $ holes b of
                          (b,b_):_ -> let (x,y) = f v vars a b in (x, b_ y)
                          [] -> (coreLam vars2 b, coreApp (CoreVar v) (map CoreVar vars2))
            where
                vars2 = collectFreeVars b \\ vars

a <<|+ b = do (bind,x) <- f (collectFreeVars b) a b
              return $ if isCoreVar x
                  then lookupJust (fromCoreVar x) bind
                  else coreLet bind x
    where
        f :: UniqueIdM m => [CoreVarName] -> CoreExpr -> CoreExpr -> m (Subst, CoreExpr)

        f vars a b | blurVar a `eq1CoreExpr` blurVar b &&
                     length (children a) == length (children b) = do
            let (as, a_) = uniplate a
                (bs, b_) = uniplate b
            (binds,cs) <- mapAndUnzipM (uncurry $ f vars) (zip as bs)
            return (concat binds, b_ cs)

        f vars a b = if null (collectFreeVars b \\ vars)
                     then do v <- getVar; return ([(v,b)], CoreVar v)
                     else return ([], b)


msgGluck a b = do
    (bind,x) <- f (collectFreeVars b) a b
    return $ if isCoreVar x
             then lookupJust (fromCoreVar x) bind
              else coreLet bind x
    where
        f :: UniqueIdM m => [CoreVarName] -> CoreExpr -> CoreExpr -> m (Subst, CoreExpr)

        f vars a b | blurVar a `eq1CoreExpr` blurVar b &&
                     length (children a) == length (children b) = do
            let (as, a_) = uniplate a
                (bs, b_) = uniplate b
            (binds,cs) <- mapAndUnzipM (uncurry $ f vars) (zip as bs)
            return (concat binds, b_ cs)

        f vars a b = if null (collectFreeVars b \\ vars)
                     then do v <- getVar; return ([(v,b)], CoreVar v)
                     else return ([], b)



-- essential idea: try and move the differences to the top of the
-- expressions so the inner bits are as similar as possible
msg2 :: UniqueIdM m => CoreExpr -> CoreExpr -> m (CoreExpr, CoreExpr)
msg2 a b = do
    (bind,a,b) <- msg2' a b
    let (vars,vals) = unzip bind
        (as,bs) = unzip vals

        f xs x = coreLet' (zip vars xs) x

    return (f as a, f bs b)
    

msg2' :: UniqueIdM m => CoreExpr -> CoreExpr -> m (SubstPair, CoreExpr, CoreExpr)
msg2' x y | not (blurVar x `eq1CoreExpr` blurVar y)
         || (length (children x) /= length (children y)) = do
    v <- getVar
    return ([(v,(x,y))], CoreVar v, CoreVar v)

-- must be free variables at this point
msg2' (CoreVar x) (CoreVar y) = return ([], CoreVar x, CoreVar y)

msg2' x y = do
    -- normalise both expressions
    i <- getIdM
    x <- duplicateExpr1 x
    putIdM i
    y <- duplicateExpr1 y
    
    -- grab bits
    let bound = fst $ uniplateBoundVars x
        (xs, x_) = uniplate x
        (ys, y_) = uniplate y

    (bind, xs2, ys2) <- liftM unzip3 $ mapM (f bound) (zip xs ys)
    return (concat bind, x_ xs2, y_ ys2)
    where
        f bound (x,y) = do
            (bind,x,y) <- msg2' x y
            let (good,bad) = partition safe bind
                safe (_,(a,b)) = (collectFreeVars a ++ collectFreeVars b) `disjoint` bound
                wrap f = coreLet' [(v,f z) | (v,z) <- bad]
            return (good, wrap fst x, wrap snd y)


duplicateExpr1 :: UniqueIdM m => CoreExpr -> m CoreExpr
duplicateExpr1 x = do
    let (cs,gen) = uniplateBoundVars x
    cs2 <- getVars (length cs)
    let rep = zip cs $ map CoreVar cs2
    return $ descend (replaceFreeVars rep) $ gen cs2


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
        f x y = any (f x) ys ||
                (x `eq1CoreExpr` y && length xs == length ys && and (zipWith f xs ys))
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


module Optimise.Evaluate(evaluate) where

import Optimise.State
import Optimise.Termination
import Optimise.Util
import Optimise.CAF

import Yhc.Core hiding (uniqueBoundVarsFunc)
import Yhc.Core.FreeVar3

import Control.Monad.State
import Data.List
import Data.Maybe
import Safe

import qualified Data.Map as Map
import qualified Data.Set as Set

---------------------------------------------------------------------
-- DATA TYPES

type Rho = [CoreExpr]
emptyRho = []

---------------------------------------------------------------------
-- DRIVER

preOpt x = transformExpr f x
    where
        f (CoreFun "Prelude;otherwise") = CoreCon "Prelude;True"
        f x = x

evaluate :: (Int -> Core -> IO ()) -> Core -> IO Core
evaluate out c = do
    cafs <- return $ detectCafs c
    out 0 c
    c <- return $ preOpt c
    out 1 c
    c <- {- liftM (coreReachable ["main"]) -} (eval cafs c)
    out 3 c
    c <- return $ coreFix c
    out 4 c
    return c

coreFix :: Core -> Core
coreFix = coreReachable ["main"] . coreInline InlineCallOnce


---------------------------------------------------------------------
-- EVAL DRIVER

eval :: Set.Set CoreFuncName -> Core -> IO Core
eval cafs core = do
    let s0 = S Map.empty [] 1 1 (coreFuncMap fm) (`Set.member` primsSet) (`Set.member` cafs) undefined
    (_,sn) <- sioRun (tieFunc (coreFuncMap fm "main")) s0
    return $ core{coreFuncs = prims ++ funcs sn}
    where
        fm = toCoreFuncMap core
        prims = filter isCorePrim (coreFuncs core)
        primsSet = Set.fromList $ map coreFuncName prims


--------------------------------------------------------------------
-- RECURSIVE TIE


addFunc :: CoreFunc -> SS ()
addFunc func = modify $ \s -> s{funcs = func : funcs s}

tieFunc :: CoreFunc -> SS ()
tieFunc func = do
    CoreFunc name args body <- uniqueBoundVarsFunc func
    body <- tie emptyRho body
    addFunc (CoreFunc name args body)


tie :: Rho -> CoreExpr -> SS CoreExpr
tie rho x = do
    (args,CoreFunc _ params x) <- return $ normalise x
    case x of
        CoreVar y -> return $ CoreVar $ head args
        x -> do
            s <- get
            let key = x
            name <- case Map.lookup key (names s) of
                Just name -> return name
                Nothing -> do
                    name <- getName x
                    modify $ \s -> s{names = Map.insert key name (names s)}
                    x <- deCaf x
                    x <- onf name rho x
                    addFunc (CoreFunc name (if null params then ["uncaf"] else params) x)
                    return name
            return $ coreApp (CoreFun name) (if null args then [CoreCon "()"] else map CoreVar args)
    where
        getName x = do
            s <- get
            put $ s{nameId = nameId s + 1}
            return $ uniqueJoin (f s x) (nameId s)

        f s (CoreFun x) = if prim s x then "f" else x
        f s (CoreApp x y) = f s x
        f s _ = "f"

        deCaf o@(CoreFun x) = do
            s <- get
            if not $ caf s x then return o else do
                CoreFunc _ params body <- uniqueBoundVarsFunc $ core s x
                return $ coreLam params body
        deCaf x = return x



-- name the variables so they are in normal form
-- return a list of the variables in order they need giving
normalise :: CoreExpr -> ([CoreVarName],CoreFunc)
normalise x = (vars, evalState (uniqueBoundVarsFunc (CoreFunc "" vars x)) (1 :: Int))
    where vars = collectFreeVars x


---------------------------------------------------------------------
-- OPTIMISATION


{-
POSTCONDITIONS:

* Any let binding spit out by unfold MUST be preserved
* All let bindings must be in ONF, and referenced more than once
* The body must be in ONF
-}


-- must invoke tie on all computations below the most optimal form
-- must try and unfold at least once

-- for each let-rhs or case-on, optimise it once
-- if you reach over (size n) then unpeel until you get to size n, and tie the remainder
--
-- resultName is only passed to aid debugging
onf :: CoreFuncName -> Rho -> CoreExpr -> SS CoreExpr
onf resultName rho x = do
    x <- coreSimplifyExprUniqueExt onfExt x
    let whistle = filter (<<| x) rho
    if not $ null whistle then do
        (t,subs) <- msg (head whistle) x
        sioPrint $ "\n\nonf whistle" ~~ x ~~ head whistle ~~ t ~~ subs
        let binds = [(v,e) | (v,(_,e)) <- subs]
            freeBinds = nub $ concatMap (collectFreeVars . snd) binds
            freeNorm = collectFreeVars x

        if True || isCoreVar t || not (null (freeBinds \\ freeNorm)) then
            unpeel rho x
         else
            unpeel rho $ coreLet binds t
     else do
        x2 <- unfold x
        if x2 == x then do
            unpeel rho x
         else
            onf resultName (x:rho) x2


-- unpeel at least one layer, but keep going if it makes no difference
unpeel :: Rho -> CoreExpr -> SS CoreExpr
unpeel rho x = do s <- get; descendM (f s) x
    where
        f s (CoreFun x) | caf s x = tie rho (CoreFun x)
        f s x = do
            x2 <- unfold x
            if x2 == x
                then descendM (f s) x
                else tie rho x


-- perform one unfolding, if you can
unfold :: CoreExpr -> SS CoreExpr
unfold (CoreCase on alts) = do on <- unfold on; return $ CoreCase on alts
unfold (CoreApp x xs) = do x <- unfold x; return $ CoreApp x xs

unfold (CoreLet ((v,e):bind) x) = do
    e2 <- unfold e
    if e == e2 then do
        x <- unfold $ coreLet bind x
        return $ CoreLet [(v,e)] x
     else do
        return $ CoreLet ((v,e2):bind) x

unfold (CoreFun x) = do
    s <- get
    if prim s x || caf s x then return $ CoreFun x else do
        CoreFunc _ params body <- uniqueBoundVarsFunc $ core s x
        return $ coreLam params body

unfold x = return x




---------------------------------------------------------------------
-- SIMPLIFICATION RULES

onfExt cont x@(CoreCase (CoreVar on) alts) | on `elem` collectFreeVars (CoreCase (CoreLit $ CoreInt 0) alts) =
        liftM (CoreCase (CoreVar on)) (mapM f alts)
    where
        f (pat@(PatCon c vs),rhs) = do
            let lhs = coreApp (CoreCon c) (map CoreVar vs)
            rhs <- transformM cont $ replaceFreeVars [(on,lhs)] rhs
            return (pat,rhs)

        f (lhs,rhs) = return (lhs,rhs)

onfExt cont o@(CoreLet bind x) | not (null ctrs) && not (isCoreLetRec o) = do
        (newbinds,oldbinds) <- mapAndUnzipM f ctrs
        transformM cont $ coreLet (concat newbinds ++ other) $ replaceFreeVars oldbinds x
    where
        (ctrs,other) = partition (isCoreCon . fst . fromCoreApp . snd) bind

        f (name,x) = do
                vs <- replicateM (length tl) getVar
                return (zip vs tl, (name, coreApp hd (map CoreVar vs)))
            where (hd,tl) = fromCoreApp x

-- be careful with letrec
onfExt cont o@(CoreLet bind x) | not (null lam) && not (isCoreLetRec o) = do
        x <- replaceFreeVarsUnique lam x
        transformM cont $ coreLet other x
    where
        (lam,other) = partition (isCoreLam . snd) bind

onfExt cont (CoreApp (CoreFun x) [CoreLit (CoreInt a), CoreLit (CoreInt b)])
        | isJust p = cont $ CoreCon $ if fromJust p a b then "Prelude;True" else "Prelude;False"
    where
        p = Map.lookup x intPrims

onfExt cont x = return x


intPrims :: Map.Map CoreFuncName (Int -> Int -> Bool)
intPrims = Map.fromList
    [("LT_W",(<))
    ,("GT_W",(>))
    ,("EQ_W",(==))
    ]

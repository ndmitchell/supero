
module Optimise.Evaluate(evaluate) where

import Optimise.State
import Optimise.Simplify
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

-- Note on naming new functions:
-- If the function is a CAF in the original, it must have the same name
-- If the function is not a CAF, it must get a uniquely new name


---------------------------------------------------------------------
-- DRIVER

-- debug hook to optimise something other than main
mainName = "main"

evaluate :: Termination -> (Int -> Core -> IO ()) -> Core -> IO Core
evaluate term out c = do
    out 0 c
    let cafs = detectCafs c
    c <- eval term cafs c
    out 1 c
    c <- return $ decaffeinate cafs c
    out 2 c
    c <- return $ coreFix c
    out 3 c
    return c

coreFix :: Core -> Core
coreFix = coreReachable [mainName] . coreInline (if agressive then InlineFull else InlineCallOnce)
    where agressive = False


---------------------------------------------------------------------
-- EVAL DRIVER

eval :: Termination -> Set.Set CoreFuncName -> Core -> IO Core
eval term cafs core = do
    let s0 = S Map.empty [] (uniqueFuncsNext core) 1 (coreFuncMap fm) (`Set.member` primsSet) (`Set.member` cafs) term
    (_,sn) <- sioRun (tieFunc mainName) s0
    return $ core{coreFuncs = prims ++ funcs sn}
    where
        fm = toCoreFuncMap core
        prims = filter isCorePrim (coreFuncs core)
        primsSet = Set.fromList $ map coreFuncName prims


--------------------------------------------------------------------
-- RECURSIVE TIE


addFunc :: CoreFunc -> SS ()
addFunc func = modify $ \s -> s{funcs = func : funcs s}


addKey :: CoreExpr -> CoreFuncName -> SS ()
addKey key val = do
    modify $ \s -> s{names = Map.insert key val (names s)}
    sioPutStr "."

tieFunc :: CoreFuncName -> SS CoreExpr
tieFunc name = do
    s <- get
    when (not (prim s name) && CoreFun name `Map.notMember` names s) $ do
        CoreFunc _ args body <- uniqueBoundVarsFunc $ core s name
        addKey (CoreFun name) name
        body <- tie emptyContext body
        addFunc (CoreFunc name args body)
    return $ CoreFun name


tie :: Context -> CoreExpr -> SS CoreExpr
tie context x = do
    (args,CoreFunc _ params x) <- return $ normalise x
    case x of
        CoreVar y -> return $ CoreVar $ head args
        CoreFun x -> tieFunc x
        x -> do
            s <- get
            let key = x
            name <- case Map.lookup key (names s) of
                Just name -> return name
                Nothing -> do
                    name <- getName x
                    addKey key name
                    x <- onf name context x
                    addFunc (CoreFunc name params x)
                    return name
            return $ coreApp (CoreFun name) (map CoreVar args)
    where
        getName x = do
            s <- get
            put $ s{nameId = nameId s + 1}
            return $ uniqueJoin (f s x) (nameId s)

        f s (CoreFun x) | not $ prim s x = x
        f s (CoreApp x y) = f s x
        f s _ = "f"


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
onf :: CoreFuncName -> Context -> CoreExpr -> SS CoreExpr
onf resultName context x = do
    s <- get
    x <- coreSimplifyExprUniqueExt simplify x
    r <- (term s) context{current=x}
    case r of
        Just x -> unpeel context x
        Nothing -> do
            x2 <- unfold x
            if x2 == x then do
                unpeel context x
             else do
                context <- return context{currents=x:currents context, rho=x:rho context}
                onf resultName context x2


-- unpeel at least one layer, but keep going if it makes no difference
-- never residuate a CoreFun, since it won't be linked otherwise
unpeel :: Context -> CoreExpr -> SS CoreExpr
unpeel context (CoreFun x) = tieFunc x
unpeel context x = descendM (tie context{currents=[]}) x


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

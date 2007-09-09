
module Evaluate5(evaluate) where

import Yhc.Core hiding (uniqueBoundVarsFunc)
import Yhc.Core.FreeVar3
import Yhc.Core.UniqueId
import Debug.Trace
import CoreUtil

import Control.Monad.State
import Control.Applicative
import Control.Arrow
import StateFail
import Data.List
import Data.Maybe
import Safe
import Termination

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap

---------------------------------------------------------------------
-- DATA TYPES

data S = S {names :: Map.Map CoreExpr CoreFuncName
           ,funcs :: [CoreFunc] -> [CoreFunc] -- difference list to make it lazy
           ,nameId :: Int
           ,uniqueId :: Int
           ,core :: CoreFuncName -> CoreFunc
           ,prim :: CoreFuncName -> Bool
           ,caf :: CoreFuncName -> Bool -- an expensive caf
           }

instance UniqueId S where
    getId = uniqueId
    putId i x = x{uniqueId = i}


-- don't use the fail, but can remove that later...
type SS a = StateFail S () a

type UnfoldId = Int
data Unfold = Unfold CoreFuncName [CoreExpr]

type CoreFuncNameInfo = String
type CoreExprInfo = CoreExpr
type Info = [UnfoldId]


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
    let s0 = S Map.empty id 1 1 (coreFuncMap fm) (`Set.member` primsSet) (`Set.member` cafs)
    sn <- sfRun (tieFunc (coreFuncMap fm "main")) s0
    case sn of
        Left i -> error $ show (i :: Int)
        Right (_,sn) -> return $ core{coreFuncs = prims ++ funcs sn []}
    where
        fm = toCoreFuncMap core
        prims = filter isCorePrim (coreFuncs core)
        primsSet = Set.fromList $ map coreFuncName prims


---------------------------------------------------------------------
-- CAF DETECTION

detectCafs :: Core -> Set.Set CoreFuncName
detectCafs core = Set.fromList [coreFuncName x | x <- coreFuncs core, isCaf (coreFuncMap fm) x]
    where fm = toCoreFuncMap core


isCaf func (CoreFunc name [] body) = expensive $ coreSimplify body
    where
        expensive (CoreCon x) = False
        expensive (CoreFun x) = False
        expensive (CoreLit x) = False
        expensive (CoreApp (CoreCon x) xs) = any expensive xs
        expensive (CoreApp (CoreFun x) xs) = not $ unsaturated func x xs
        expensive x = error $ show ("missed",x)

isCaf _ _ = False


unsaturated :: (CoreFuncName -> CoreFunc) -> CoreFuncName -> [CoreExpr] -> Bool
unsaturated func name args = f [] name (length args)
    where
        f seen name args | name `elem` seen = False
                         | args == 0 || arity > args = True
                         | isCoreFunc x = g (name:seen) (coreFuncBody x) (args - arity)
                         | otherwise = False
            where
                x = func name
                arity = coreFuncArity x

        g seen (CoreApp (CoreFun name) args) extra = f seen name (length args + extra)
        g seen (CoreFun name) extra = f seen name extra
        g seen (CorePos _ x) extra = g seen x extra
        g _ _ _ = False


---------------------------------------------------------------------
-- RECURSIVE TIE


addFunc :: CoreFunc -> SS ()
addFunc func = modify $ \s -> s{funcs = funcs s . (func:)}

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
        sfPrint $ "\n\nonf whistle" ~~ x ~~ head whistle ~~ t ~~ subs
        if isCoreVar t then
            descendM (tie rho) x
         else
            descendM (tie rho) $ coreLet [(v,e) | (v,(_,e)) <- subs] t
     else do
        x2 <- unfold x
        if x2 == x then do
            descendM (tie rho) x
         else
            onf resultName (x:rho) x2


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


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
import System.IO

import qualified Data.Map as Map
import qualified Data.Set as Set

-- Note on naming new functions:
-- If the function is a CAF in the original, it must have the same name
-- If the function is not a CAF, it must get a uniquely new name


---------------------------------------------------------------------
-- DRIVER

-- debug hook to optimise something other than main
mainName = "main"

evaluate :: Handle -> Termination -> (Int -> Core -> IO ()) -> Core -> IO Core
evaluate h term out c = do
    out 0 c
    let cafs = detectCafs c
    c <- eval h term cafs c
    out 1 c
    c <- return $ coreReachable [mainName] $ coreInline InlineForward c
    out 2 c
    c <- return $ decaffeinate cafs c
    out 3 c
    c <- return $ coreFix c
    out 4 c
    return c

coreFix :: Core -> Core
coreFix = coreReachable [mainName] . coreInline (if agressive then InlineFull else InlineCallOnce)
    where agressive = True


---------------------------------------------------------------------
-- EVAL DRIVER

eval :: Handle -> Termination -> Set.Set CoreFuncName -> Core -> IO Core
eval h term cafs core = do
    let s0 = S Map.empty [] (uniqueFuncsNext core) 1 (coreFuncMap fm) (`Set.member` primsSet) (`Set.member` cafs) term h
    (_,sn) <- sioRun (tieFunc mainName) s0
    --putStrLn $ unlines $ map show $ Map.toList $ names sn
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
        -- small enough to "pre-inline"
        CoreLit x | isCoreLitSmall x -> return $ CoreLit x
        CoreCon x -> return $ CoreCon x

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
                    x <- optimise context x
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

-- either you have no term (residuate root)
-- or are running with a termination embedding
-- or are running with a perfect root
data Score = None | Term | Root
             deriving (Eq,Ord,Show)

pick (a,b) (c,d) = if c > a then (c,d) else (a,b)


-- optimise an expression until you are told to stop
optimise :: Context -> CoreExpr -> SS CoreExpr
optimise context x = opt context (None,undefined) [return x]

opt context (n,best) [] = unpeel context best

opt context best (x:xs) = do
    s <- get
    x <- x
    x <- simplifyFull x
    if badUnfold s x then opt context (pick best (Root,x)) xs else do
        r <- (term s) context{current=x}
        case r of
            Just x -> opt context (pick best (Term,x)) xs
            Nothing -> opt (addContext context x) (None,x) (unfolds s x)


-- unpeel at least one layer, but keep going if it makes no difference
-- never residuate a CoreFun, since it won't be linked otherwise
unpeel :: Context -> CoreExpr -> SS CoreExpr
unpeel context (CoreFun x) = tieFunc x
unpeel context x = descendM f x
    where
        f (CoreFun x) = tieFunc x
        f x = do
            s <- get
            if badUnfold s x
                then descendM f x
                else tie context{currents=[]} x


-- return all the possible unfoldings that could be carried out
-- ensure that you call simplify afterwards
unfolds :: S -> CoreExpr -> [SS CoreExpr]
unfolds s x = [g f y | (CoreFun y,f) <- contexts x, canUnfold s y]
    where
        g context name = do
            CoreFunc _ params body <- uniqueBoundVarsFunc $ core s name
            return $ context $ coreLam params body


-- return True if any possible unfolding is uselss
-- having badUnfold implies you should residuate straight away
-- i.e. case PRIM of ... may unfold in the leaves, but will never unfold back
-- ditto for CON x y z, you will never change the Con
badUnfold :: S -> CoreExpr -> Bool
badUnfold s (CoreCase x _) = null $ unfolds s x
badUnfold s (CoreLam _ _) = True
badUnfold s (CoreApp (CoreCon _) _) = True
badUnfold s (CoreApp (CoreFun x) _) = not $ canUnfold s x
badUnfold s _ = False


-- can you unfold a particular function
canUnfold :: S -> CoreFuncName -> Bool
canUnfold s x = not $ prim s x || caf s x

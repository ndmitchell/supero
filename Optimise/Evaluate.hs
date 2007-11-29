
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
                    x <- coreSimplifyExprUniqueExt simplify x
                    x <- onf context x
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


-- optimise an expression until you are told to stop
onf :: Context -> CoreExpr -> SS CoreExpr
onf context x = do
    s <- get
    r <- if badUnfold s x
         then return $ Just x
         else (term s) context{current=x}
    case r of
        Just x2 -> do
            unpeel context x2
        Nothing ->
            case unfolds s x of
                [] -> unpeel context x
                zs -> do
                    context <- return $ addContext context x
                    zs <- sequence zs
                    zs <- mapM (score context) zs
                    let x2 = snd $ head $ sortBy (compare `on` fst) zs
                    onf context x2

    where
        score context x = do
            s <- get
            if badUnfold s x then return (1,x) else do
                stop <- (term s) context{current=x}
                return ((if isNothing stop then 0 else 2), x)


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
            let x = context $ coreLam params body
            coreSimplifyExprUniqueExt simplify x


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


module Evaluate(evaluate) where

import Yhc.Core hiding (
    uniqueBoundVarsCore, uniqueBoundVarsFunc, uniqueBoundVars,
    collectAllVars, collectFreeVars, replaceFreeVars)
import Yhc.Core.FreeVar3
import Debug.Trace

import Control.Monad.State
import Data.List
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap


-- record the quota for each function, initially all start at
-- default quota, then decrement on each call
-- 0 means cannot inline further
type Env = (Int, IntMap.IntMap (Map.Map CoreFuncName Int))

-- 0 would result in no optimisation
-- 1 does not work, since to get the recursion optimised
--   you need to inline the function, then one recursive call
-- 2 is still low, but more than 1
defaultQuota = 6 :: Int


emptyEnv :: Env
emptyEnv = (1, IntMap.singleton 0 Map.empty)


canInline :: Env -> Int -> CoreFuncName -> Bool
canInline (_,e) i s = (> 0) $ Map.findWithDefault 1 s $ fromJust $ IntMap.lookup i e

doneInline :: Env -> Int -> CoreFuncName -> (Int,Env)
doneInline (u,e) i s = (u, (u+1, IntMap.insert u new e))
    where
        old = fromJust $ IntMap.lookup i e
        new = Map.insert s (Map.findWithDefault defaultQuota s old - 1) old




evaluate :: Core -> Core
evaluate = coreFix . eval . inlineLambda . eval

inlineLambda core = transformExpr f core
    where
        names = Map.fromList [(name,coreLam args body)
                | (CoreFunc name args body) <- coreFuncs core, isCoreLam body || isCoreInt body]

        f (CoreFun x) = Map.findWithDefault (CoreFun x) x names
        f x = x



exclude = ["Prelude.Prelude.Prelude.1107.showPosInt"
          ,"Prelude.Prelude.Prelude.1108.showPosInt"
          ,"Prelude.Prelude.Prelude.1111.showPosInt"]


data S = S {names :: Map.Map CoreExpr CoreFuncName
           ,funcs :: [CoreFunc] -> [CoreFunc] -- difference list to make it lazy
           ,nameId :: Int
           ,core :: CoreFuncName -> CoreFunc
           ,prim :: CoreFuncName -> Bool
           }

coreFix :: Core -> Core
coreFix = coreReachable ["main"] . coreInline InlineCallOnce


type SS a = State S a

eval :: Core -> Core
eval core = core{coreFuncs = prims ++ funcs s []}
    where
        s = execState f s0
        s0 = S Map.empty id 1 (coreFuncMap fm) isPrim
        f = tieFunc (coreFuncMap fm "main")

        fm = toCoreFuncMap core
        prims = filter isCorePrim (coreFuncs core)
        primsSet = Set.fromList $ map coreFuncName prims
        isPrim x = if ':' `elem` x then error $ "isPrim on: " ++ x else x `Set.member` primsSet


addFunc :: CoreFunc -> SS ()
addFunc x = modify $ \s -> s{funcs = funcs s . (x:)}

tieFunc :: CoreFunc -> SS ()
tieFunc (CoreFunc name args body) = do
    body <- tie emptyEnv (annotate 0 body)
    addFunc (CoreFunc name args body)


tie :: Env -> CoreExpr -> SS CoreExpr
tie seen x = do
    (args,CoreFunc _ params x) <- normalise x
    key <- return $ represent x
    s <- get
    case x of
        CoreVar y -> return $ head args
        _ -> do
            name <- case Map.lookup key (names s) of
                Just name -> return name
                Nothing -> do
                    name <- getName x
                    modify $ \s -> s{names = Map.insert key name (names s)}
                    x <- optimise seen x
                    addFunc (CoreFunc name params x)
                    return name
            liftM (CoreApp (CoreFun name)) $ mapM (optHead seen) args
    where
        -- getName (CoreFun x) = return x
        getName x = do
            s <- get
            put $ s{nameId = nameId s + 1}
            return $ uniqueJoin (f s x) (nameId s)

        f s (CoreFun x) = if prim s name then "f" else name
            where name = snd $ splitFuncName x
        f s (CoreApp x y) = f s x
        f s _ = "f"


represent :: CoreExpr -> CoreExpr
represent = transform f
    where
        f (CoreFun x) = CoreFun $ snd $ splitFuncName x
        f x = x

-- lift out all primitives to the top level
-- name the variables so they are in normal form
normalise :: CoreExpr -> SS ([CoreExpr], CoreFunc)
normalise x = do
    s <- get
    return $ flip evalState (1 :: Int) $ do

        -- first lift out all primitive bindings which:
        -- 1) depend only on free variables (for correctness)
        -- 2) are fully saturated (for first order)
        x <- return $ transform wrapFun x
        let vars = freeVars 'v' \\ collectAllVars x
            free = collectFreeVars x
            ps = zip vars [o | o@(CoreApp (CoreFun fn) as) <- universe x
                             , let (num,name) = splitFuncName fn
                             , let uses = collectFreeVars o, all (`elem` free) uses
                             , prim s name, coreFuncArity (core s name) == length as
                             ]
        x <- return $ transform (dePrim ps) x

        -- next order the free vars and prims in a normal order
        let free = collectFreeVars x
        func <- uniqueBoundVarsFunc $ CoreFunc "" free x

        -- finally, put back the variables        
        let f x = fromMaybe (CoreVar x) $ lookup x ps
        return (map f free, func)
    where
        wrapFun (CoreFun x) = CoreApp (CoreFun x) []
        wrapFun (CoreApp (CoreApp x y) z) = CoreApp x (y++z)
        wrapFun x = x

        dePrim ps o@(CoreApp (CoreFun fn) xs) =
            case lookupRev o ps of
                Nothing -> o
                Just y -> CoreVar y
        dePrim ps x = x


lookupRev :: Eq key => key -> [(val,key)] -> Maybe val
lookupRev key [] = Nothing
lookupRev key ((v,k):vk) | key == k = Just v
                         | otherwise = lookupRev key vk

{-
ONF : Optimised Normal Form

onf = let bind in onf
    | case var of
    | case prim of
    | var
    | prim
    | con
-}

optimise :: Env -> CoreExpr -> SS CoreExpr
optimise seen x = do
    s <- get
    (seen,x) <- return $ evalState (uniqueBoundVars x >>= onf s seen) (1::Int)
    optHead seen x


optHead :: Env -> CoreExpr -> SS CoreExpr
optHead seen x = do
        s <- get
        (bind,x) <- return $ fromCoreLet x
        x <- case x of
            CoreCase on alts -> do
                on <- f s on
                alts <- liftM (zip (map fst alts)) $ mapM (f s . snd) alts
                return $ CoreCase on alts

            CoreFun x | prim s name -> return $ CoreFun name
                      | otherwise -> tieFunc (core s name) >> return (CoreFun name)
                where (num,name) = splitFuncName x
            
            _ -> descendM (f s) x

        bind <- liftM (zip (map fst bind)) $ mapM (f s . snd) bind
        return $ coreLet bind x
    where
        f s (CoreApp (CoreFun x) xs) | prim s name = liftM (CoreApp (CoreFun name)) (mapM (f s) xs)
            where (num,name) = splitFuncName x

        f s (CoreFun x) | prim s name = return $ CoreFun name
            where (num,name) = splitFuncName x

        f s x | isRoot s x = descendM (f s) x
              | otherwise  = tie seen x


isRoot :: S -> CoreExpr -> Bool
isRoot s x | isCoreVar x || isCoreCon x || isCoreConst x = True
isRoot s (CoreFun x) | primFunc s x = True
isRoot s _ = False


{-
To acheive ONF need to do standard simplification rules, plus
if the root is a function, expand it.

Functions may be wrapped in case, or in let.
-}


onf :: S -> Env -> CoreExpr -> State Int (Env,CoreExpr)
onf s seen x = f seen x
    where
        f seen x = do
            x <- coreSimplifyExprUniqueExt onfExt x
            let o = x
            (_let , x) <- return $ unwrapLet  x
            (_case, x) <- return $ unwrapCase x
            (_app , x) <- return $ unwrapApp  x
            case x of
                CoreFun x | not (prim s name) ->
                    if name `notElem` exclude {- canInline seen num name -} then do
                        (i,seen) <- return $ doneInline seen num name
                        CoreFunc _ args body <- uniqueBoundVarsFunc $ core s name
                        f seen $ _let $ _case $ _app $ CoreLam args $ annotate i body
                    else
                        trace ("Skipping " ++ name) $ return (seen,o)
                    where
                        (num,name) = splitFuncName x
                    
                _ -> return (seen,o)


onfExt cont x@(CoreCase (CoreVar on) alts) | on `elem` collectFreeVars (CoreCase (CoreInt 0) alts) =
        liftM (CoreCase (CoreVar on)) (mapM f alts)
    where
        f (lhs,rhs) = do
            rhs <- transformM cont $ replaceFreeVars [(on,lhs)] rhs
            return (lhs,rhs)

onfExt cont (CoreLet bind x) | not $ null lam =
        transformM cont $ coreLet other $ replaceFreeVars lam x
    where
        (lam,other) = partition (isCoreLam . snd) bind

onfExt cont (CoreApp (CoreFun x) [CoreInt a,CoreInt b])
        | isJust p = cont $ CoreCon $ if fromJust p a b then "Prelude.True" else "Prelude.False"
    where
        p = Map.lookup (snd $ splitFuncName x) intPrims

onfExt cont x = return x


intPrims :: Map.Map CoreFuncName (Int -> Int -> Bool)
intPrims = Map.fromList
    [("LT_W",(<))
    ,("GT_W",(>))
    ]



unwrapLet (CoreLet x y) = (CoreLet x,y)
unwrapLet x = (id,x)

unwrapCase (CoreCase x y) = (flip CoreCase y,x)
unwrapCase x = (id,x)

unwrapApp (CoreApp x y) = (flip CoreApp y,x)
unwrapApp x = (id,x)


primFunc s = prim s . snd . splitFuncName


splitFuncName :: CoreFuncName -> (Int,String)
splitFuncName x = (read a, b)
    where (a,_:b) = break (== ':') x


annotate :: Int -> CoreExpr -> CoreExpr
annotate i = transform f
    where
        f (CoreFun x) = CoreFun (show i ++ ":" ++ x)
        f x = x


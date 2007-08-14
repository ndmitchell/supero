
module Evaluate2(evaluate) where

import Yhc.Core hiding (
    uniqueBoundVarsCore, uniqueBoundVarsFunc, uniqueBoundVars,
    collectAllVars, collectFreeVars, replaceFreeVars)
import Yhc.Core.FreeVar3
import Debug.Trace

import Control.Monad.State
import Control.Arrow
import StateFail
import Data.List
import Data.Maybe
import Safe

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap


funcBound = 2 :: Int


-- for each (function,argpos) pair record:
-- 1) the number of items this represents
-- 2) the maximum permitted argument size
--
-- the first n calls are allowed, regardless of size
type Env = Map.Map (CoreFuncName,Int) (Int,Int)


emptyEnv :: Env
emptyEnv = Map.empty


getEnv :: Env -> (CoreFuncName,Int) -> Int
getEnv env key = case Map.lookup key env of
                     Nothing -> maxBound
                     Just (a,b) | a > 0 -> maxBound
                                | otherwise -> b

setEnv :: Env -> CoreFuncName -> [Int] -> Env
setEnv env func args = f env [((func,i),a) | (i,a) <- zip [0..] args]
    where
        f env ((key,val):rest) = f (Map.insertWith g key (funcBound,val) env) rest
        f env [] = env

        g (val,_) (a,b) | a > 0 = (a-1,max val b)
                        | otherwise = (0,min val b)


{-
canInline :: Env -> Int -> CoreFuncName -> Bool
canInline (_,e) i s = (> 0) $ Map.findWithDefault 1 s $ fromJust $ IntMap.lookup i e

doneInline :: Env -> Int -> CoreFuncName -> (Int,Env)
doneInline (u,e) i s = (u, (u+1, IntMap.insert u new e))
    where
        old = fromJust $ IntMap.lookup i e
        new = Map.insert s (Map.findWithDefault defaultQuota s old - 1) old
-}


preOpt x = transformExpr f x
    where
        f (CoreFun "Prelude;otherwise") = CoreCon "Prelude;True"
        f x = x


safety = [("Prelude;1111_showPosInt",1)
         ,("Prelude;foldl",1)
         ,("Prelude;_foldr",1)
         ,("Prelude;_filter",0)
         ]


evaluate :: (Int -> Core -> IO ()) -> Core -> IO Core
evaluate out c = do
    out 1 c
    c <- return $ preOpt c
    out 2 c
    c <- return $ eval c
    out 3 c
    c <- return $ coreFix c
    out 4 c
    return c


inlineLambda core = transformExpr f core
    where
        names = Map.fromList [(name,coreLam args body)
                | (CoreFunc name args body) <- coreFuncs core, isCoreLam body || isCoreLit body]

        f (CoreFun x) = Map.findWithDefault (CoreFun x) x names
        f x = x



exclude = ["Prelude.Prelude.Prelude.1107.showPosInt"
          ,"Prelude.Prelude.Prelude.1108.showPosInt"
          ,"Prelude.Prelude.Prelude.1111.showPosInt"
          ]


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
eval core = core{coreFuncs = prims ++ funcs sn []}
    where
        sn = execState (tieFunc (coreFuncMap fm "main")) s0
        s0 = S Map.empty id 1 (coreFuncMap fm) (`Set.member` primsSet)

        fm = toCoreFuncMap core
        prims = filter isCorePrim (coreFuncs core)
        primsSet = Set.fromList $ map coreFuncName prims


addFunc :: CoreFunc -> SS ()
addFunc (CoreFunc name args body) = modify $ \s -> s{funcs = funcs s . (CoreFunc name args body:)}

tieFunc :: CoreFunc -> SS ()
tieFunc (CoreFunc name args body) = do
    body <- tie emptyEnv body
    addFunc (CoreFunc name args body)


tie :: Env -> CoreExpr -> SS CoreExpr
tie seen x = do
    (args,CoreFunc _ params x) <- normalise x
    s <- get
    case x of
        CoreVar y -> return $ head args
        _ -> do
            name <- case Map.lookup x (names s) of
                Just name -> return name
                Nothing -> do
                    name <- getName x
                    modify $ \s -> s{names = Map.insert x name (names s)}
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

        f s (CoreFun x) = if prim s x then "f" else x
        f s (CoreApp x y) = f s x
        f s _ = "f"



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
            ps = zip vars $ concatMap (shouldLift x s free) $ universe x
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

        dePrim ps o =
            case lookupRev o ps of
                Nothing -> o
                Just y -> CoreVar y
        dePrim ps x = x

        shouldLift _ _ _ _ = []

{-
        shouldLift _ s free o@(CoreApp (CoreFun fn) as)
                | all (`elem` free) uses && prim s name && coreFuncArity (core s name) == length as
                = [o]
            where
                (num,name) = splitFuncName fn
                uses = collectFreeVars o

        shouldLift orig s free o@(CoreApp x as)
                | not (null fn) && num `elem` skip s = [o]
            where
                fn = coreName x
                (num,name) = splitFuncNameNote ("shouldlift" ++ show orig) fn

        shouldLift _ s free _ = []
-}

        coreName (CoreCon x) = x
        coreName (CoreFun x) = x
        coreName _ = ""



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
optimise env x = do
    s <- get
    (env,x) <- return $ evalState (uniqueBoundVars x >>= onf s env) 1
    optHead env x


optHead :: Env -> CoreExpr -> SS CoreExpr
optHead seen x = do
        s <- get
        (bind,x) <- return $ fromCoreLet x
        x <- case x of
            CoreCase on alts -> do
                on <- f s on
                alts <- liftM (zip (map fst alts)) $ mapM (f s . snd) alts
                return $ CoreCase on alts

            CoreFun x | prim s x -> return $ CoreFun x
                      | otherwise -> tieFunc (core s x) >> return (CoreFun x)
            
            _ -> descendM (f s) x

        bind <- liftM (zip (map fst bind)) $ mapM (f s . snd) bind
        return $ coreLet bind x
    where
        f s (CoreApp (CoreFun x) xs) | prim s x = liftM (CoreApp (CoreFun x)) (mapM (f s) xs)

        f s (CoreFun x) | prim s x = return $ CoreFun x

        f s x | isRoot s x = descendM (f s) x
              | otherwise  = tie seen x


isRoot :: S -> CoreExpr -> Bool
isRoot s x | isCoreVar x || isCoreCon x || isCoreLit x = True
isRoot s (CoreFun x) | prim s x = True
isRoot s _ = False


{-
To acheive ONF need to do standard simplification rules, plus
if the root is a function, expand it.

Functions may be wrapped in case, or in let.
-}


exprSize :: CoreExpr -> Int
exprSize = length . universe

exprSizeOld :: CoreExpr -> Int
exprSizeOld = fold (\_ cs -> 1 + maximum (0:cs))




comparing x = on compare x

on f g x y = f (g x) (g y)

onf :: S -> Env -> CoreExpr -> State Int (Env,CoreExpr)
onf s env x = f [] env x
    where
        f bound env x = do
            x <- coreSimplifyExprUniqueExt onfExt x
            let o = x
            (binds, x) <- return $ fromCoreLet x
            (_case, x) <- return $ unwrapCase x
            () <- if exprSizeOld o > 25 then error $ show o ++ "\nSize Overflow!" else return ()
            case fromCoreApp x of
                (CoreFun x, args) | not (prim s x) -> do
                    let now = map (exprSize . replaceFreeVars binds) args
                        lim = [getEnv env (x,i) | i <- [0..length args - 1]]

                        evil = [a | (n,l,a) <- zip3 now lim args, n - 4 > l]
                        env2 = setEnv env x now

                    vars <- replicateM (length evil) getVar
                    let free = collectFreeVars (CoreApp (CoreCon "") evil)
                        (freezebind,movebind) = partition ((`elem` free) . fst) binds
                        newbind = zip vars evil
                        newbound = newbind ++ freezebind ++ bound

                    -- () <- if x == "Prelude;1111_showPosInt" then trace (show (now,old,args,binds)) $ return () else return ()
                    () <- if null evil then return () else trace ("Recursive call bigger: " ++ show o) $ return ()

                    let args2 = [maybe a CoreVar (lookupRev a newbind) | a <- args]

                    CoreFunc _ params body <- uniqueBoundVarsFunc $ core s x
                    f newbound env2 $ coreLet movebind $ _case $ coreApp (CoreLam params body) args2
                    
                _ -> return (env, coreLet bound o)


onfExt cont x@(CoreCase (CoreVar on) alts) | on `elem` collectFreeVars (CoreCase (CoreLit $ CoreInt 0) alts) =
        liftM (CoreCase (CoreVar on)) (mapM f alts)
    where
        f (pat@(PatCon c vs),rhs) = do
            let lhs = coreApp (CoreCon c) (map CoreVar vs)
            rhs <- transformM cont $ replaceFreeVars [(on,lhs)] rhs
            return (pat,rhs)

        f (lhs,rhs) = return (lhs,rhs)

onfExt cont (CoreLet bind x) | not $ null lam =
        transformM cont $ coreLet other $ replaceFreeVars lam x
    where
        (lam,other) = partition (isCoreLam . snd) bind

onfExt cont (CoreApp (CoreFun x) [CoreLit (CoreInt a), CoreLit (CoreInt b)])
        | isJust p = cont $ CoreCon $ if fromJust p a b then "Prelude;True" else "Prelude;False"
    where
        p = Map.lookup x intPrims

{-
onfExt cont (CoreCase on alts) | isCoreCon a && con `elem` lhs =
        cont $ CoreCase (coreApp (CoreCon con) b) alts
    where
        lhs = [c | (PatCon c _, _) <- alts]
        con = snd $ splitFuncName $ fromCoreCon a
        (a,b) = fromCoreApp on
-}

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


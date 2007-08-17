
module Evaluate2(evaluate) where

import Yhc.Core
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

---------------------------------------------------------------------
-- DATA TYPES

data S = S {names :: Map.Map CoreExpr CoreFuncName
           ,funcs :: [CoreFunc] -> [CoreFunc] -- difference list to make it lazy
           ,unfolds :: IntMap.IntMap {- UnfoldId -} Unfold
           ,nameId :: Int
           ,uniqueId :: Int
           ,core :: CoreFuncName -> CoreFunc
           ,prim :: CoreFuncName -> Bool
           }

-- don't use the fail, but can remove that later...
type SS a = StateFail S () a

type UnfoldId = Int
data Unfold = Unfold CoreFuncName [CoreExpr]

type CoreFuncNameInfo = String
type CoreExprInfo = CoreExpr
type Info = [UnfoldId]


---------------------------------------------------------------------
-- DRIVER

preOpt x = transformExpr f x
    where
        f (CoreFun "Prelude;otherwise") = CoreCon "Prelude;True"
        f x = x

evaluate :: (Int -> Core -> IO ()) -> Core -> IO Core
evaluate out c = do
    out 1 c
    c <- return $ preOpt c
    out 2 c
    c <- eval c
    out 3 c
    c <- return $ coreFix c
    out 4 c
    return c

coreFix :: Core -> Core
coreFix = coreReachable ["main"] . coreInline InlineCallOnce


---------------------------------------------------------------------
-- INFO STUFF

getInfo :: CoreFuncNameInfo -> (CoreFuncName,Info)
getInfo x = (b, read a)
    where (a,_:b) = break (== '@') x

putInfo :: CoreFuncName -> Info -> CoreFuncNameInfo
putInfo a b = show b ++ "@" ++ a


---------------------------------------------------------------------
-- UNFOLD STUFF

unfold :: CoreFuncNameInfo -> [CoreExprInfo] -> SS (Maybe ([(CoreVarName,CoreExprInfo)], CoreExprInfo))
unfold _ _ = return Nothing


---------------------------------------------------------------------
-- EVAL DRIVER

eval :: Core -> IO Core
eval core = do
    let s0 = S Map.empty id 1 (coreFuncMap fm) (`Set.member` primsSet)
    sn <- sfRun (tieFunc (coreFuncMap fm "main")) s0
    case sn of
        Left i -> error $ show (i :: Int)
        Right (_,sn) -> return $ core{coreFuncs = prims ++ funcs sn []}
    where
        fm = toCoreFuncMap core
        prims = filter isCorePrim (coreFuncs core)
        primsSet = Set.fromList $ map coreFuncName prims


---------------------------------------------------------------------
-- RECURSIVE TIE


addFunc :: CoreFunc -> SS ()
addFunc func = modify $ \s -> s{funcs = funcs s . (func:)}

tieFunc :: CoreFunc -> SS ()
tieFunc (CoreFunc name args body) = do
    body <- tie body
    addFunc (CoreFunc name args body)


tie :: CoreExpr -> SS CoreExpr
tie x = do
    (args,CoreFunc _ params x) <- return $ normalise x
    case normalise x of
        CoreVar y -> return $ head args
        x -> do
            s <- get
            name <- case Map.lookup x (names s) of
                Just name -> return $ CoreFun name
                Nothing -> do
                    name <- getName x
                    modify $ \s -> s{names = Map.insert x name (names s)}
                    x <- optimise seen x
                    addFunc (CoreFunc name params x)
                    return name
            return $ coreApp (CoreFun name) args
    where
        getName x = do
            s <- get
            put $ s{nameId = nameId s + 1}
            return $ uniqueJoin (f s x) (nameId s)

        f s (CoreFun x) = if prim s x then "f" else x
        f s (CoreApp x y) = f s x
        f s _ = "f"



-- name the variables so they are in normal form
-- return a list of the variables in order they need giving
normalise :: CoreExpr -> ([CoreVarName],CoreFunc)
normalise x = (vars, evalState (uniqueBoundVarsFunc (CoreFunc "" vars x)) (1 :: Int))
    where vars = collectFreeVars x


---------------------------------------------------------------------
-- OPTIMISATION


fixM f x = do
    x2 <- f x
    if x == x2 then return x2 else fixM f x2

{-
ALGORITHM:

Must start at the top-most let, and must optimise it as best as you can

Then move down the let chains, if any give rise to any fixed let's
must also promote the let's they depend on into fixed let's

Once you get to the final thing, you have fixed and dynamic lets, and
optimise as normal.

Do not call tie on the let variables until the end.
-}


-- must invoke tie on all computations below the most optimal form
onf :: CoreExpr -> SS CoreExpr
onf x = do
        x <- coreSimplifyExprUniqueExt onfExt x
        g fromCoreLetDeep x

    where
        f (fixed,x) = do
            x <- coreSimplifyExprUniqueExt onfExt x
            (bind,x) <- return $ fromCoreLetDeep x
            g fixed bind x

        g fixed done (t:odo) x = do
            (fixed2,x) <- h x


        g fixed done [] x = do



        -- take an expression, return a list of new fixed
        -- and the optimised expression
        h x = do
            (_case,x) <- return $ unwrapCase x
            s <- get
            case fromCoreApp x of
                (CoreFun x, args) | not (prim s x) ->
                    res <- unfold x args
                    case res of
                        Nothing -> 




        f (fixed,free,x) =
            case fromCoreLetDeep x of
                ([],x) ->
                    




    


            let o = x
            (binds, x) <- return $ fromCoreLetDeep x
            --binds <- mapM (\(a,b) -> do b <- f bound env b; return (a,snd b)) binds
            --x <- coreSimplifyExprUniqueExt onfExt (coreLet binds x)
            --(binds, x) <- return $ fromCoreLet x
            (_case, x) <- return $ unwrapCase x
            () <- if exprSizeOld o > 25 then error $ show o ++ "\nSize Overflow!" else return ()
            case fromCoreApp x of
                (CoreFun x, args) | not (prim s x) -> do
                    let now = map (exprSize . replaceFreeVars binds) args
                        lim = [getEnv env (x,i) | i <- [0..length args - 1]]

                        evil = [] :: [CoreExpr]  -- [a | (n,l,a) <- zip3 now lim args, n - 4 > l]
                        env2 = setEnv env x now

                    vars <- replicateM (length evil) getVar
                    let free = collectFreeVars (CoreApp (CoreCon "") evil)
                        (freezebind,movebind) = partition ((`elem` free) . fst) binds
                        newbind = zip vars evil
                        newbound = newbind ++ freezebind ++ bound

                    -- () <- if x == "Prelude;1111_showPosInt" then trace (show (now,old,args,binds)) $ return () else return ()
                    () <- if null evil then return () else trace ("Recursive call bigger: " ++ show evil ++ "\n" ++ show o) $ return ()

                    let args2 = [maybe a CoreVar (lookupRev a newbind) | a <- args]

                    CoreFunc _ params body <- uniqueBoundVarsFunc $ core s x
                    f newbound env2 $ coreLet movebind $ _case $ coreApp (CoreLam params body) args2

                (CoreVar lhs, args) | isJust $ lookup lhs binds -> do
                    let Just rhs = lookup lhs binds
                    (env,rhs) <- f [] env rhs
                    let binds2 = filter ((/= lhs) . fst) binds
                    if inlineLetBind rhs
                        then f bound env $ replaceFreeVars [(lhs,rhs)] $ coreLet binds2 $ _case $ coreApp rhs args
                        else return (env, coreLet bound $ coreLet ((lhs,rhs):binds2) $ _case x)

                _ -> return (env, coreLet bound $ coreLet binds $ _case x)





-- take an expression, and optimise it so that it is in ONF
optimise :: CoreExprInfo -> SS CoreExprInfo
optimise env x = do
    s <- get
    sfPrint "=== FROM =========================================="
    sfPrint (show x)
    (env,x) <- return $ evalState (uniqueBoundVars x >>= onf s env) 1
    sfPrint "--- TO   ------------------------------------------"
    sfPrint (show x)
    ans <- liftIO $ getChar
    () <- if ans /= '\n' then error "done" else return ()
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

For all subexpressions that result, call tie on them
-}


onf :: S -> Env -> CoreExpr -> State Int (Env,CoreExpr)
onf s env x = f [] env x
    where
        f bound env x = do
            x <- coreSimplifyExprUniqueExt onfExt x
            let o = x
            (binds, x) <- return $ fromCoreLetDeep x
            --binds <- mapM (\(a,b) -> do b <- f bound env b; return (a,snd b)) binds
            --x <- coreSimplifyExprUniqueExt onfExt (coreLet binds x)
            --(binds, x) <- return $ fromCoreLet x
            (_case, x) <- return $ unwrapCase x
            () <- if exprSizeOld o > 25 then error $ show o ++ "\nSize Overflow!" else return ()
            case fromCoreApp x of
                (CoreFun x, args) | not (prim s x) -> do
                    let now = map (exprSize . replaceFreeVars binds) args
                        lim = [getEnv env (x,i) | i <- [0..length args - 1]]

                        evil = [] :: [CoreExpr]  -- [a | (n,l,a) <- zip3 now lim args, n - 4 > l]
                        env2 = setEnv env x now

                    vars <- replicateM (length evil) getVar
                    let free = collectFreeVars (CoreApp (CoreCon "") evil)
                        (freezebind,movebind) = partition ((`elem` free) . fst) binds
                        newbind = zip vars evil
                        newbound = newbind ++ freezebind ++ bound

                    -- () <- if x == "Prelude;1111_showPosInt" then trace (show (now,old,args,binds)) $ return () else return ()
                    () <- if null evil then return () else trace ("Recursive call bigger: " ++ show evil ++ "\n" ++ show o) $ return ()

                    let args2 = [maybe a CoreVar (lookupRev a newbind) | a <- args]

                    CoreFunc _ params body <- uniqueBoundVarsFunc $ core s x
                    f newbound env2 $ coreLet movebind $ _case $ coreApp (CoreLam params body) args2

                (CoreVar lhs, args) | isJust $ lookup lhs binds -> do
                    let Just rhs = lookup lhs binds
                    (env,rhs) <- f [] env rhs
                    let binds2 = filter ((/= lhs) . fst) binds
                    if inlineLetBind rhs
                        then f bound env $ replaceFreeVars [(lhs,rhs)] $ coreLet binds2 $ _case $ coreApp rhs args
                        else return (env, coreLet bound $ coreLet ((lhs,rhs):binds2) $ _case x)

                _ -> return (env, coreLet bound $ coreLet binds $ _case x)


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

onfExt cont (CoreLet bind x) | not $ null lam = do
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
    ]


---------------------------------------------------------------------
-- CORE UTILITIES

unwrapLet (CoreLet x y) = (CoreLet x,y)
unwrapLet x = (id,x)

unwrapCase (CoreCase x y) = (flip CoreCase y,x)
unwrapCase x = (id,x)

unwrapApp (CoreApp x y) = (flip CoreApp y,x)
unwrapApp x = (id,x)


inlineLetBind (CoreLit{}) = True
inlineLetBind (CoreLam{}) = True
inlineLetBind _ = False


fromCoreLetDeep (CoreLet x y) = (x++a,b)
    where (a,b) = fromCoreLetDeep y
fromCoreLetDeep x = ([],x)

exprSize :: CoreExpr -> Int
exprSize = length . universe

exprSizeOld :: CoreExpr -> Int
exprSizeOld = fold (\_ cs -> 1 + maximum (0:cs))

comparing x = on compare x

on f g x y = f (g x) (g y)


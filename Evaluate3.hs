
module Evaluate3(evaluate) where

import Yhc.Core hiding (uniqueBoundVarsFunc)
import Yhc.Core.FreeVar3
import Yhc.Core.UniqueId
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

unfoldBound = 8 :: Int


-- given that these arguments happened previously, shall we blur this one?
-- say yes if this call is a superset of one of the previous calls
blur :: CoreExpr -> [CoreExpr] -> Bool
blur this prev = concatMap universe (children (blurVar this)) `overlap` map blurVar prev
    where
        blurVar = transform f
        f (CoreVar _) = CoreVar ""
        f x = x

overlap a b = any (`elem` b) a


-- rule 1, do not allow more than n recursive unfoldings of something
unfold :: CoreFuncNameInfo -> [CoreExprInfo] -> SS (Maybe ([(CoreVarName,CoreExprInfo)], CoreExprInfo))
unfold x args = do
        let (name,info) = getInfo x
        s <- get
        let recs = map (\i -> fromJust $ IntMap.lookup i (unfolds s)) info
            prev = [as | Unfold x as <- recs, x == name]

        if length prev >= unfoldBound
            then do
                sfPrint $ "Warning, aborted on " ++ name ++ " " ++ show prev
                return Nothing
            else do
                let blurs = zipWith blur (map unannotate args) (transpose prev)
                (binds,newargs) <- liftM unzip $ sequence $ zipWith g (blurs ++ repeat False) args

                let newid = IntMap.size (unfolds s)
                    newinfo = newid : info
                modify $ \s -> s{unfolds = IntMap.insert newid (Unfold name (map unannotate newargs)) (unfolds s)}

                CoreFunc _ params body <- uniqueBoundVarsFunc $ core s name
                body <- return $ transform (f newinfo) body
                let expr = coreApp (coreLam params body) newargs
                return $ Just (concat binds, expr)
    where
        f info (CoreFun x) = CoreFun (putInfo x info)
        f info x = x

        g True arg = do
            v <- getVar
            return ([(v,arg)], CoreVar v)
        g False arg = return ([], arg)


annotate :: CoreExpr -> CoreExprInfo
annotate = transform f
    where
        f (CoreFun x) = CoreFun (putInfo x [])
        f x = x


unannotate :: CoreExprInfo -> CoreExpr
unannotate = transform f
    where
        f (CoreFun x) = CoreFun (fst $ getInfo x)
        f x = x


---------------------------------------------------------------------
-- EVAL DRIVER

eval :: Core -> IO Core
eval core = do
    let s0 = S Map.empty id IntMap.empty 1 1 (coreFuncMap fm) (`Set.member` primsSet)
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
tieFunc func = do
    CoreFunc name args body <- uniqueBoundVarsFunc func
    body <- tie $ annotate body
    addFunc (CoreFunc name args body)


tie :: CoreExpr -> SS CoreExpr
tie x = do
    (args,CoreFunc _ params x) <- return $ normalise x
    case x of
        CoreVar y -> return $ CoreVar $ head args
        x -> do
            s <- get
            let key = unannotate x
            name <- case Map.lookup key (names s) of
                Just name -> return name
                Nothing -> do
                    name <- getName x
                    modify $ \s -> s{names = Map.insert key name (names s)}
                    x <- onf x
                    addFunc (CoreFunc name params x)
                    return name
            return $ coreApp (CoreFun name) (map CoreVar args)
    where
        getName x = do
            s <- get
            put $ s{nameId = nameId s + 1}
            return $ uniqueJoin (f s x) (nameId s)

        f s (CoreFun x) = if prim s name then "f" else name
            where name = fst $ getInfo x
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
onf :: CoreExpr -> SS CoreExpr
onf x = do
        x <- coreSimplifyExprUniqueExt onfExt x
        x <- fRep x
        onfTie =<< unprotect x
    where
        fRep x = do
            x2 <- f [] x
            if x == x2 then return x2 else fRep x2

        -- control the ordering so that let's get done in the right order
        f done x = do
            (binds,x) <- return $ fromCoreLetDeep x
            case binds of
                [] -> g $ coreLet done x
                (name,val):bs -> do
                    (binds,val) <- liftM fromCoreLetDeep $ g $ coreLet done val
                    f (binds ++ [(name,val)]) (coreLet bs x)

        -- optimise something
        g x = do
            x <- coreSimplifyExprUniqueExt onfExt x
            let o = x
            (binds,x) <- return $ fromCoreLetDeep x
            (_case,x) <- return $ unwrapCase x
            s <- get
            case fromCoreApp x of
                (CoreFun x, args) | not (primInfo s x) && x /= protectName -> do
                    res <- unfold x args
                    case res of
                        Nothing -> return o
                        Just (newbinds,x) -> do
                            binds <- return $ binds ++ map (id *** protect) newbinds
                            g $ coreLet binds $ _case x
                _ -> return o


protectName = "PROTECT!"

protect x = CoreApp (CoreFun protectName) [x]

-- find all the protect names, and hoist them up as let's (if not already there)
-- remove all protect markers
unprotect :: CoreExpr -> SS CoreExpr
unprotect x = do
        let (a,b) = fromCoreLet x
            items = concatMap universe $ concatMap (children . snd) a ++ [b]
            prot = [x | CoreApp (CoreFun p) (x:_) <- items, p == protectName]

        names <- replicateM (length prot) getVar
        x <- return $ transform (f (zip prot names)) x
        return $ transform g $ coreLet (zip names prot) x
    where
        f ren o@(CoreApp (CoreFun p) (x:xs)) | p == protectName =
            case lookup x ren of
                Nothing -> o
                Just y -> CoreApp (CoreFun p) (CoreVar y:xs)
        f ren x = x

        g (CoreApp (CoreFun p) (x:xs)) | p == protectName =
            coreApp x xs
        g x = x




-- call tie on all subexpressions that need optimising
-- this code smells bad, refactoring required
onfTie :: CoreExpr -> SS CoreExpr
onfTie x = do
        s <- get
        (bind,x) <- return $ fromCoreLet x
        x <- case x of
            CoreCase on alts -> do
                on <- f s on
                alts <- liftM (zip (map fst alts)) $ mapM (f s . snd) alts
                return $ CoreCase on alts

            CoreFun x | prim s name -> return $ CoreFun name
                      | otherwise -> tieFunc (core s name) >> return (CoreFun name)
                where name = fst $ getInfo x

            _ -> descendM (f s) x

        bind <- liftM (zip (map fst bind)) $ mapM (f s . snd) bind
        return $ coreLet bind x
    where
        f s (CoreApp (CoreFun x) xs) | prim s name = liftM (CoreApp (CoreFun name)) (mapM (f s) xs)
            where name = fst $ getInfo x

        f s (CoreFun x) | prim s name = return $ CoreFun name
            where name = fst $ getInfo x

        f s x | isRoot s x = descendM (f s) x
              | otherwise  = tie x


isRoot :: S -> CoreExpr -> Bool
isRoot s x | isCoreVar x || isCoreCon x || isCoreLit x = True
isRoot s (CoreFun x) | primInfo s x = True
isRoot s _ = False


primInfo s = prim s . fst . getInfo
coreInfo s = core s . fst . getInfo


{-










                        
unfold :: CoreFuncNameInfo -> [CoreExprInfo] -> SS (Maybe ([(CoreVarName,CoreExprInfo)], CoreExprInfo))


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

            
        
            g 
    
        f fixed free x = do
            (binds,x) <- return $ fromCoreLetDeep x
            case binds of
                [] -> do
                    





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
-}


{-
To acheive ONF need to do standard simplification rules, plus
if the root is a function, expand it.

Functions may be wrapped in case, or in let.

For all subexpressions that result, call tie on them
-}

{-

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

-}

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

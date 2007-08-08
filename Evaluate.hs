
module Evaluate(evaluate) where

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



preOpt x = transformExpr f x
    where
        f (CoreFun "Prelude;otherwise") = CoreCon "Prelude;True"
        f x = x


safety = [("Prelude;1111_showPosInt",1)
         ,("Prelude;foldl",1)
         ]


evaluate :: Core -> Core
evaluate = coreFix . unprotect . eval . inlineLambda . eval . preOpt . protect safety

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
           ,skip :: [Int]
           }

coreFix :: Core -> Core
coreFix = coreReachable ["main"] . coreInline InlineCallOnce



type SS a = StateFail S Int a


fromRight (Right x) = x


eval :: Core -> Core
eval core_orig = core_orig{coreFuncs = prims ++ f s0}
    where
        core = annotate isPrim core_orig

        f s = case sfRun (tieFunc (coreFuncMap fm "main")) s of
                  Left i -> f s{skip = i : skip s}
                  Right (_,s) -> funcs s []

        s0 = S Map.empty id 1 (coreFuncMap fm) isPrim []
        fm = toCoreFuncMap core
        prims = filter isCorePrim (coreFuncs core)
        primsSet = Set.fromList $ map coreFuncName prims
        isPrim x = if ':' `elem` x then error $ "isPrim on: " ++ x else x `Set.member` primsSet


addFunc :: CoreFunc -> SS ()
addFunc (CoreFunc name args body) = modify $ \s -> s{funcs = funcs s . (CoreFunc name args (unannotate body):)}

tieFunc :: CoreFunc -> SS ()
tieFunc (CoreFunc name args body) = do
    body <- tie emptyEnv body
    addFunc (CoreFunc name args body)


size :: CoreExpr -> Int
size x = foldr max 0 (map size $ children x) + 1


seqList x = length x `seq` x

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
            ps = zip vars $ concatMap (shouldLift x s free) $ universeCase x
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
optimise seen x = do
    s <- get
    case sfRun (uniqueBoundVars x >>= onf s seen) (1::Int) of
        Left i -> sfFail (i :: Int)
        Right ((seen,x),_) -> optHead seen x


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
isRoot s x | isCoreVar x || isCoreCon x || isCoreLit x = True
isRoot s (CoreFun x) | primFunc s x = True
isRoot s _ = False


{-
To acheive ONF need to do standard simplification rules, plus
if the root is a function, expand it.

Functions may be wrapped in case, or in let.
-}


exprSize :: CoreExpr -> Int
exprSize = fold (\_ cs -> 1 + maximum (0:cs)) . transform f
    where
        f (CoreLet bind x) = replaceFreeVars bind x
        f x = x


repeats :: CoreFuncName -> CoreExpr -> [String]
repeats name = f ["Prelude.:" | name == "Prelude.Prelude.Prelude.1111.showPosInt"] . transform remLet
    where
        remLet (CoreLet bind x) = replaceFreeVars bind x
        remLet x = x

        f seen (CoreApp x xs) = f seen x ++ concatMap (f new) xs
            where new = seen ++ concatMap g (universe x)
        f seen (CoreFun x) = [x | snd (splitFuncName x) `elem` seen, snd (splitFuncName x) /= "typeRealWorld"]
        f seen (CoreCon x) = [x | x `elem` seen]
        f seen x = concatMap (f seen) (children x)

        g (CoreFun x) = [snd $ splitFuncName x]
        g (CoreCon x) = [x]
        g x = []


pickBreak :: S -> CoreExpr -> Int
pickBreak s x =
    snd $ maximumBy (comparing fst) $ map (length &&& head) $
    group $ sort $ filter (`notElem` skip s) $
        concat [[i,i,i] | CoreCon c <- universeCase x, let i = fst $ splitFuncName c] ++
        [fst $ splitFuncName c | CoreFun c <- universe x]


universeCase o@(CoreCase on alts) = o : concatMap universeCase (on:map snd alts)
universeCase x = x : concatMap universeCase (children x)


comparing x = on compare x

on f g x y = f (g x) (g y)

onf :: S -> Env -> CoreExpr -> StateFail Int Int (Env,CoreExpr)
onf s seen original = f [] seen original
    where
        f done seen x = do
            x <- coreSimplifyExprUniqueExt onfExt x
            let o = x
            (_let , x) <- return $ unwrapLet  x
            (_case, x) <- return $ unwrapCase x
            let size = exprSize o
            let x2 = x
            (_app , x) <- return $ unwrapApp  x
            case x of
                CoreFun x | not (prim s name) ->
                    --let rep = repeats x $ _let x2 in
                    --if size > 10 && null rep then
                    --    error $ show o
                    if size > 20 then do
                        let common = pickBreak s o
                        () <- trace (show o) $ return ()
                        () <- trace ("Break on: " ++ show common) $ return ()
                        sfFail common
                    else if True || name `notElem` exclude then do
                        -- () <- if size > 4 then trace ("Inlining at size " ++ show size ++ ", " ++ x) $ return () else return ()
                        --() <- if size == 8 then error $ show o else return ()
                        --() <- if rep then trace ("Repeated, " ++ x) (return ()) else return ()
                        --() <- trace ("Inlining " ++ name) $ return ()
                        (i,seen) <- return $ doneInline seen num name
                        CoreFunc _ args body <- uniqueBoundVarsFunc $ core s name
                        r <- f (name:done) seen $ _let $ _case $ _app $ CoreLam args body
                        --() <- if name `elem` exclude then error $ show $ snd r else return ()
                        () <- if x `elem` done then error $ show (name,original,_let $ _case $ _app $ CoreFun x) else return ()
                        return r
                    else
                        --error $ show o
                        trace ("Skipping " ++ name) $ return (seen,o)
                    where
                        (num,name) = splitFuncName x
                    
                _ -> return (seen,o)


onfExt cont x@(CoreCase (CoreVar on) alts) | on `elem` collectFreeVars (CoreCase (CoreLit $ CoreInt 0) alts) =
        liftM (CoreCase (CoreVar on)) (mapM f alts)
    where
        f (pat@(PatCon c vs),rhs) = do
            let lhs = coreApp (CoreCon ("0:"++c)) (map CoreVar vs)
            rhs <- transformM cont $ replaceFreeVars [(on,lhs)] rhs
            return (pat,rhs)

        f (lhs,rhs) = return (lhs,rhs)

onfExt cont (CoreLet bind x) | not $ null lam =
        transformM cont $ coreLet other $ replaceFreeVars lam x
    where
        (lam,other) = partition (isCoreLam . snd) bind

onfExt cont (CoreApp (CoreFun x) [CoreLit (CoreInt a), CoreLit (CoreInt b)])
        | isJust p = cont $ CoreCon $ if fromJust p a b then "0:Prelude;True" else "0:Prelude;False"
    where
        p = Map.lookup (snd $ splitFuncName x) intPrims

onfExt cont (CoreCase on alts) | isCoreCon a && con `elem` lhs =
        cont $ CoreCase (coreApp (CoreCon con) b) alts
    where
        lhs = [c | (PatCon c _, _) <- alts]
        con = snd $ splitFuncName $ fromCoreCon a
        (a,b) = fromCoreApp on

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


splitFuncNameNote :: String -> CoreFuncName -> (Int,String)
splitFuncNameNote s x = (readNote ("splitFuncName: " ++ s) a, b)
    where (a,_:b) = break (== ':') x

splitFuncName :: CoreFuncName -> (Int,String)
splitFuncName x = (readNote "splitFuncName" a, b)
    where (a,_:b) = break (== ':') x


unannotate :: CoreExpr -> CoreExpr
unannotate = transform f
    where
        f (CoreCon x) = CoreCon $ snd $ splitFuncName x
        f x = x


-- annotate each function call with i:...
annotate :: (CoreFuncName -> Bool) -> Core -> Core
annotate isPrim core = core{coreFuncs = evalState (mapM g (coreFuncs core)) 1}
    where
        g x | isCoreFunc x = do
            bod <- f $ coreFuncBody x
            return $ x{coreFuncBody=bod}
        g x = return x

        f (CoreFun x) = do
            i <- get
            put (i+1)
            return $ CoreFun $ show i ++ ":" ++ x

        f (CoreCon x) = do
            i <- get
            put (i+1)
            return $ CoreCon $ show i ++ ":" ++ x

        f (CoreCase on alts) = do
            on2 <- f on
            let (lhs,rhs) = unzip alts
            rhs2 <- mapM f rhs
            return $ CoreCase on2 (zip lhs rhs2)

        f x = descendM f x



protect :: [(CoreFuncName,Int)] -> Core -> Core
protect pos x = x{coreFuncs = idFunc : map f (coreFuncs x)}
    where
        idFunc = CorePrim "id" 1 "" "" True []

        f (CoreFunc name args body) = CoreFunc name args2 (coreLet binds body)
            where
                args2 = [if i `elem` as then v else a | (i,a,v) <- zip3 [0..] args vars]
                binds = [(a,CoreApp (CoreFun "id") [CoreVar v]) | (i,a,v) <- zip3 [0..] args vars, i `elem` as]

                vars = freeVars 'v' \\ (args ++ collectAllVars body)
                as = [i | (n,i) <- pos, n == name]

        f x = x


unprotect :: Core -> Core
unprotect = transformExpr f
    where
        f (CoreApp (CoreFun "id") [x]) = x
        f x = x

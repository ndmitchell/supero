
module Evaluate4(evaluate) where

import Yhc.Core hiding (uniqueBoundVarsFunc)
import Yhc.Core.FreeVar3
import Yhc.Core.UniqueId
import Debug.Trace

import Control.Monad.State
import Control.Applicative
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
    c <- return $ smallSize c
    out 2 c
    c <- liftM (coreReachable ["main"]) (eval cafs c)
    out 3 c
    c <- return $ coreFix c
    out 4 c
    return c

coreFix :: Core -> Core
coreFix = coreReachable ["main"] . coreInline InlineCallOnce



-- make sure all function bodies have a size of <= 2
-- to make sure the size is not overflowed too fast
smallSize core = core{coreFuncs = fst $ execState (mapM_ f (coreFuncs core)) ([], uniqueFuncsNext core)}
    where
        f (CoreFunc name args body) = do
            body <- h name body
            modify $ \(a,b) -> (CoreFunc name args body:a,b)
        f x = modify $ \(a,b) -> (x:a,b)

        h name x | size x <= 2 = return x
                 | otherwise = do
            body <- descendM (h name) x
            let free = collectFreeVars body
            (a,b) <- get
            let newname = uniqueJoin name b
            put (CoreFunc newname free body:a, b+1)
            return $ coreApp (CoreFun newname) (map CoreVar free)


--------------------------------------------------------------------
-- CRAZY ORDERING

-- f (g a) > f a, where g is some nonempty wrapping
-- ignoring variable names


(!>!) :: CoreExpr -> CoreExpr -> Bool
(!>!) a b = ba /= bb && peel ba bb
    where
        ba = blurVar a
        bb = blurVar b

        -- peel away a common shell
        peel a b | a == b = True
        peel a b | nas == nbs && _a vs == _b vs =
                inclusion a b || and (zipWith peel as bs)
            where
                vs = replicate nas (CoreVar "")
                (nas, nbs) = (length as, length bs)
                (as, _a) = uniplate a
                (bs, _b) = uniplate b
        peel a b = inclusion a b

        inclusion a b = b `elem` universe a



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
    body <- tie body
    addFunc (CoreFunc name args body)


tie :: CoreExpr -> SS CoreExpr
tie x = do
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
                    let o = x
                    x <- onf name x
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



-- name the variables so they are in normal form
-- return a list of the variables in order they need giving
normalise :: CoreExpr -> ([CoreVarName],CoreFunc)
normalise x = (vars, evalState (uniqueBoundVarsFunc (CoreFunc "" vars x)) (1 :: Int))
    where vars = collectFreeVars x


---------------------------------------------------------------------
-- OPTIMISATION


maxSize = 5

size :: CoreExpr -> Int
size = fold (\_ i -> 1 + maximum (0:i))


{-
POSTCONDITIONS:

* Any let binding spit out by unfold MUST be preserved
* All let bindings must be in ONF, and referenced more than once
* The body must be in ONF
-}


-- must invoke tie on all computations below the most optimal form

-- for each let-rhs or case-on, optimise it once
-- if you reach over (size n) then unpeel until you get to size n, and tie the remainder
--
-- resultName is only passed to aid debugging
onf :: CoreFuncName -> CoreExpr -> SS CoreExpr
onf resultName x = do
        x <- coreSimplifyExprUniqueExt onfExt x
        
        -- if you are optimising a CAF, unfold it exactly ONCE
        s <- get
        x <- case x of
                CoreFun name | caf s name -> do
                    CoreFunc _ [] body <- uniqueBoundVarsFunc $ core s name
                    return body
                _ -> return x

        f x
    where
        f x = do
            s <- get
            r <- protect x
            case r of
                Just x -> return x
                Nothing -> do
                    r <- onfStep s x
                    case r of
                        Nothing -> unpeel s x
                        Just x -> do
                            x <- coreSimplifyExprUniqueExt onfExt x
                            if overflow x
                                then unpeel s x
                                else f x

overflow x = size x > maxSize

-- done s x = optimal s x || size x > maxSize



-- optimise to one step, Nothing says you are done already
onfStep :: S -> CoreExpr -> SS (Maybe CoreExpr)
onfStep s (CoreLet [] x) = onfStep s x

onfStep s (CoreLet ((lhs,rhs):bind) x) = do
    r <- onfStep s rhs
    case r of
        Just rhs2 -> return $ Just $ CoreLet ((lhs,rhs2):bind) x
        Nothing -> do
            r <- onfStep s $ coreLet bind x
            case r of
                Nothing -> return Nothing
                Just x2 -> return $ Just $ CoreLet [(lhs,rhs)] x2

onfStep s (CoreCase x xs) = do
    r <- onfStep s x
    case r of
        Just x2 -> return $ Just $ CoreCase x2 xs
        Nothing -> return Nothing

onfStep s (CoreApp x xs) = do
    r <- onfStep s x
    case r of
        Just x2 -> return $ Just $ coreApp x2 xs
        Nothing -> return Nothing

onfStep s (CoreFun x) | not (prim s x || caf s x) = do
    CoreFunc _ params body <- uniqueBoundVarsFunc $ core s x
    return $ Just $ coreLam params body

onfStep _ _ = return Nothing



{-
optimal s (CoreLet bind x) = optimal s x
optimal s (CoreCase on alts) = optimal s on
optimal s (CoreApp x xs) = optimal s x
optimal s (CoreFun x) = prim s x || caf s x
optimal s _ = True


unfold s (CoreLet bind x) = do
    bind <- mapM (\(a,b) -> (,) a <$> unfold s b) bind
    x <- unfold s x
    return $ CoreLet bind x

unfold s (CoreCase on alts) = do
    on <- unfold s on
    return $ CoreCase on alts

unfold s (CoreFun x) = unfold s (CoreApp (CoreFun x) [])

unfold s (CoreApp (CoreFun name) args)
    | not (prim s name) && not (caf s name) = do
    CoreFunc _ params body <- uniqueBoundVarsFunc $ core s name
    return $ coreApp (coreLam params body) args

unfold s x = return x
-}


unpeel s (CoreFun x) | caf s x = tie (CoreFun x)
unpeel s x | overflow x = descendM (unpeel s) x
           | otherwise = do
                r <- onfStep s x
                case r of
                    Nothing -> descendM (unpeel s) x
                    Just _ -> tie x


--------------------------------------------------------------------
-- PROTECTION

-- if you see f (f a), then make it a let, and spit out
-- all the code up until that point
protect :: CoreExpr -> SS (Maybe CoreExpr)
protect x = do
    let evils = filter isEvil $ concatMap universe $ children x
    if null evils then return Nothing else do liftM Just (dump (nub evils) x)


dump evils x = f (map (collectFreeVars &&& id) evils) x
    where
        f [] x = tie x
        f evil x | map snd evil `disjoint` universe x = tie x
                 | otherwise = do
            let fv = collectFreeVars x
                (now,later) = partition (\(fv2,e) -> null (fv2 \\ fv)) evil
            binds <- mapM (\(_,e) -> do v <- getVar; return (v,e)) now
            if null binds
                then descendM (f evil) x
                else descendM (f later) (CoreLet binds (rep binds x))

        rep vs x = case lookupRev x vs of
            Nothing -> descend (rep vs) x
            Just y -> CoreVar y


---------------------------------------------------------------------
-- EVIL SPOTTING

isEvil :: CoreExpr -> Bool
isEvil = any (uncurry eqContexts) . tail . getContexts


eqContexts :: CoreExpr -> CoreExpr -> Bool
eqContexts (CoreVar "") _ = True
eqContexts (CoreVar _) (CoreVar _) = True
eqContexts x y = eq1 x y && and (zipWith eqContexts (children x) (children y))


getContexts :: CoreExpr -> [(CoreExpr, CoreExpr)]
getContexts x = (CoreVar "", x) :
        [(context (map fst pre ++ [fst m] ++ map fst post), snd m)
        | (pre,mid,post) <- splits (map (id &&& getContexts) children)
        , m <- snd mid]
    where (children,context) = uniplate x

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

fixM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
fixM f x = do
    x2 <- f x
    if x == x2 then return x2 else fixM f x2

-- need to blur all uses and definitions
blurVar = transform f
    where
        f (CoreVar _) = CoreVar ""
        f (CoreLet bind x) = CoreLet (map ((,) "" . snd) bind) x
        f (CoreCase on alts) = CoreCase on [(g a,b) | (a,b) <- alts]
        f (CoreLam x y) = CoreLam (map (const "") x) y
        f x = x

        g (PatCon x _) = PatCon x []
        g x = x

-- are two constructors equal to depth 1
eq1 :: CoreExpr -> CoreExpr -> Bool
eq1 x y = length xs == length ys && _x vs == _y vs
    where
        vs = replicate (length xs) (CoreVar "")
        (xs,_x) = uniplate x
        (ys,_y) = uniplate y


splits :: [a] -> [([a],a,[a])]
splits [] = []
splits (x:xs) = ([],x,xs) : [(x:a,b,c) | (a,b,c) <- splits xs]


lookupRev :: Eq b => b -> [(a,b)] -> Maybe a
lookupRev x ((a,b):xs) | x == b = Just a
                       | otherwise = Nothing
lookupRev _ _ = Nothing


disjoint xs ys = all (`notElem` xs) ys

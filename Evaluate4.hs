
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

eval :: Core -> IO Core
eval core = do
    let s0 = S Map.empty id 1 1 (coreFuncMap fm) (`Set.member` primsSet) (`Set.member` cafs)
    sn <- sfRun (tieFunc (coreFuncMap fm "main")) s0
    case sn of
        Left i -> error $ show (i :: Int)
        Right (_,sn) -> return $ core{coreFuncs = prims ++ funcs sn []}
    where
        fm = toCoreFuncMap core
        prims = filter isCorePrim (coreFuncs core)
        primsSet = Set.fromList $ map coreFuncName prims
        cafs = Set.fromList [coreFuncName x | x <- coreFuncs core, isCaf (coreFuncMap fm) x]


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
                    x <- onf x
                    addFunc (CoreFunc name params x)
                    return name
            return $ coreApp (CoreFun name) (map CoreVar args)
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


maxSize = 8

size x = fold (\_ i -> 1 + maximum (0:i)) $ transform f x
    where
        f (CoreLet bind x) = replaceFreeVars bind x
        f x = x


{-
POSTCONDITIONS:

* Any let binding spit out by unfold MUST be preserved
* All let bindings must be in ONF, and referenced more than once
* The body must be in ONF
-}


-- must invoke tie on all computations below the most optimal form

-- for each let-rhs or case-on, optimise it once
-- if you reach over (size n) then unpeel until you get to size n, and tie the remainder
onf :: CoreExpr -> SS CoreExpr
onf x = do
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
            x <- unfold s x
            x <- coreSimplifyExprUniqueExt onfExt x
            if done s x then unpeel s x else f x

done s x = optimal s x || size x > maxSize


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


unpeel s (CoreFun x) | caf s x = tie (CoreFun x)
unpeel s x | done s x = descendM (unpeel s) x
           | otherwise = tie x




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

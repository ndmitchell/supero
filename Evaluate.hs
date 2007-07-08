
module Evaluate(evaluate) where

import Yhc.Core hiding (
    uniqueBoundVarsCore, uniqueBoundVarsFunc, uniqueBoundVars,
    collectAllVars, collectFreeVars)
import Yhc.Core.FreeVar3
import Debug.Trace

import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set


exclude = ["Prelude.Prelude.Prelude.1107.showPosInt"]


data S = S {names :: Map.Map CoreExpr CoreFuncName
           ,funcs :: [CoreFunc] -> [CoreFunc] -- difference list to make it lazy
           ,nameId :: Int
           ,core :: CoreFuncName -> CoreFunc
           ,prim :: CoreFuncName -> Bool
           }

coreFix :: Core -> Core
coreFix = coreReachable ["main"] . coreInline InlineCallOnce


type SS a = State S a

evaluate :: Core -> Core
evaluate core = coreFix $ core{coreFuncs = prims ++ funcs s []}
    where
        s = execState f s0
        s0 = S Map.empty id 1 (coreFuncMap fm) (`Set.member` primsSet)
        f = mapM_ tieFunc [func | name <- "main":exclude, Just func <- [coreFuncMapMaybe fm name]]

        fm = toCoreFuncMap core
        prims = filter isCorePrim (coreFuncs core)
        primsSet = Set.fromList $ map coreFuncName prims


addFunc :: CoreFunc -> SS ()
addFunc x = modify $ \s -> s{funcs = funcs s . (x:)}

tieFunc :: CoreFunc -> SS ()
tieFunc (CoreFunc name args body) = do
    body <- tie body
    addFunc (CoreFunc name args body)


tie :: CoreExpr -> SS CoreExpr
tie x = do
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
                    x <- optimise x
                    addFunc (CoreFunc name params x)
                    return name
            liftM (CoreApp (CoreFun name)) $ mapM optHead args
    where
        getName x = do
            s <- get
            put $ s{nameId = nameId s + 1}
            return $ uniqueJoin (f x) (nameId s)

        f (CoreFun x) = x
        f (CoreApp x y) = f x
        f _ = "f"


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
            ps = zip vars [o | o@(CoreApp (CoreFun fn) as) <- universe x, prim s fn
                             , let uses = collectFreeVars o, all (`elem` free) uses
                             , coreFuncArity (core s fn) == length as]
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

optimise :: CoreExpr -> SS CoreExpr
optimise x = do
    s <- get
    x <- return $ evalState (uniqueBoundVars x >>= onf s) (1::Int)
    optHead x


optHead :: CoreExpr -> SS CoreExpr
optHead x = do
        s <- get
        case x of
            CoreCase on alts -> do
                on <- f s on
                alts <- liftM (zip (map fst alts)) $ mapM (f s . snd) alts
                return $ CoreCase on alts
            _ -> descendM (f s) x
    where
        f s (CoreApp (CoreFun x) xs) | prim s x = liftM (CoreApp (CoreFun x)) (mapM (f s) xs)
        f s x | isRoot s x = descendM (f s) x
              | otherwise  = tie x


isRoot :: S -> CoreExpr -> Bool
isRoot s x | isCoreVar x || isCoreCon x || isCoreConst x = True
isRoot s (CoreFun x) | prim s x = True
isRoot s _ = False


isOnf :: S -> CoreExpr -> Bool
isOnf s (CoreLet _ x) = isOnf s x
isOnf s (CoreCase x _) = isCoreVar x || isPrim s x
isOnf s x = isCoreVar x || isPrim s x || isCoreCon (fst $ fromCoreApp x)

isPrim s (CoreApp (CoreFun x) _) = prim s x
isPrim s (CoreFun x) = prim s x
isPrim s _ = False

{-
To acheive ONF need to do standard simplification rules, plus
if the root is a function, expand it.

Functions may be wrapped in case, or in let.
-}


onf :: S -> CoreExpr -> State Int CoreExpr
onf s x = f x
    where
        f x = do
            x <- coreSimplifyExprUnique x
            let o = x
            (_let , x) <- return $ unwrapLet  x
            (_case, x) <- return $ unwrapCase x
            (_app , x) <- return $ unwrapApp  x
            case x of
                CoreFun x | not (prim s x) && x `notElem` exclude -> do
                    CoreFunc _ args body <- uniqueBoundVarsFunc $ core s x
                    f $ _let $ _case $ _app $ CoreLam args body
                _ -> return o



unwrapLet (CoreLet x y) = (CoreLet x,y)
unwrapLet x = (id,x)

unwrapCase (CoreCase x y) = (flip CoreCase y,x)
unwrapCase x = (id,x)

unwrapApp (CoreApp x y) = (flip CoreApp y,x)
unwrapApp x = (id,x)



{-    
        g (CoreFun x) = g (CoreApp (CoreFun x) [])

        g (CoreApp (CoreFun x) xs) = do
            CoreFunc _ args body <- uniqueBoundVarsFunc $ core s x
            f $ coreApp (CoreLam args body) xs

        g (CoreCase (CoreFun x) y) = g (CoreCase (CoreApp (CoreFun x) []) y)

        g (Core        
            

        f (CoreCase o x) = do
            o <- f o
            f (CoreCase o x)
-}


{-            


evalMain :: Core -> State Int Core
evalMain core = do
    i <- eval core (coreFuncBody $ coreFunc core "main")
    error $ show i


eval :: Core -> CoreExpr -> State Int CoreExpr
eval core x = case x of
    CoreApp x y -> do
        x <- cont x
        case x of
            CoreApp x1 x2 -> cont $ CoreApp x1 (x2++y)
            _ -> error $ show ("app here",x)

    _ -> error $ show ("end",x)
    
    where
        cont = eval core
-}
{-

    CoreCase on alts -> do
        i <- 
    
        
        f x
    where
        f (CoreCase on alts) = do
            i <- eval 
    
        func = isCoreFunc . coreFunc core
-}    
{-
        f (CoreCase (CoreApp (CoreFun fn) _) ((_,rhs):_)) | not $ func fn = f rhs
        
        f (CoreApp (CoreFun fn) xs) | func fn = uncurry coreLam $ coreInlineFuncLambda (coreFunc core fn) xs
        f (CoreFun fn) = f (CoreApp (CoreFun fn) [])
        f o@(CoreCase x y) | isCoreCon $ fst $ fromCoreApp x = coreSimplifyCaseCon o
        f o@(CoreCase (CoreCase x y) z) = coreSimplifyCaseCase o
        f (CoreApp (CoreApp x y) z) = f $ CoreApp x (y++z)
        f (CoreApp (CoreCase x y) z) = CoreCase x [(a,CoreApp b z) | (a,b) <- y]
        f (CoreCase x y) = CoreCase (f x) y
        
        
        f x = error $ show x

-}
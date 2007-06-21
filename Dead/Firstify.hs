
module Firstify(firstify) where

import Yhc.Core hiding (collectAllVars,collectFreeVars,uniqueBoundVars,replaceFreeVars)
import Yhc.Core.FreeVar2

import Firstify.Template
import Firstify.Prepare

import Data.List
import Data.Char
import Data.Maybe

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map

{-
SPECIALISE ALGORITHM

Need to generate a specialised version if:
* f gets called with more arguments than its arity
* any argument is higher order

The specialised version has:
* a free variable for each non-ho argument
* the free variables within a function, for a ho argument
-}



---------------------------------------------------------------------
-- DRIVER

-- if the first result is not null, an error occurred
-- the second result is how far you got
firstify :: Core -> Core
firstify core = coreReachable ["main"] $ coreSimplify $ fromCoreFuncMap core $ trans $ prepare core



data Spec = Spec {specId :: Int
                 ,specCore :: CoreFuncMap
                 ,specMap :: Map.Map (CoreFuncName,Template) CoreFuncName
                 -- the list of items which are currently under evaluation
                 ,specActive :: Set.Set CoreFuncName
                 -- the functions which got finished
                 ,specDone :: Set.Set CoreFuncName
                 -- those which were asked for while in active
                 ,specMissed :: Set.Set CoreFuncName
                 }

type SpecM a = State Spec a



trans :: CoreFuncMap -> CoreFuncMap
trans fm = evalState f newSpec
    where
        newSpec = Spec 1 fm Map.empty Set.empty Set.empty Set.empty
        
        mainArgs = take (length $ coreFuncArgs $ coreFuncMap fm "main") $ freeVars 'v'
        
        prims = [x | CorePrim x _ <- Map.elems fm]
        
        f = do lam prims (CoreApp (CoreFun "main") (map CoreVar mainArgs))
               s <- get
               if any (isHO . coreFuncBody . coreFuncMap (specCore s)) $ Set.toList (specMissed s)
                   then put s{specActive=Set.empty, specDone=Set.empty, specMissed=Set.empty} >> f
                   else return $ specCore s


coreFuncBody2 (CoreFunc _ _ x) = x



-- should look at the CoreFunc to see if its a primitive
lam :: [CoreFuncName] -> CoreExpr -> SpecM CoreExpr
lam prims (CoreApp (CoreFun f) xs)
    | f `elem` ["Prelude.error","error"] = liftM (CoreApp (CoreFun f)) (mapM (lam prims) $ take 1 xs)
    | f `elem` prims = liftM (CoreApp (CoreFun f)) (mapM (lam prims) xs)
    | otherwise = do
    xs <- mapM (lam prims) xs
    s <- get
    let func = coreFuncMap (specCore s) f

    -- make sure that the templating is done
    (f,xs) <- case useTemplate func xs of
        Nothing -> return (f,xs)
        Just (template,args) -> do
            s <- get
            case Map.lookup (f,template) (specMap s) of
                Just f2 -> return (f2,args)
                Nothing -> do
                    let newname = dropDollar f ++ "$" ++ show (specId s)
                    put s{specId = specId s + 1
                         ,specCore = Map.insert newname (genTemplate template func newname) (specCore s)
                         ,specMap = Map.insert (f,template) newname (specMap s)}
                    return (newname,args)

    -- now try and do the transformation on it if required
    when (not (Set.member f (specDone s)) && not (Set.member f (specActive s))) $ do
        s <- get
        put s{specActive = Set.insert f (specActive s)}
        let func = coreFuncMap (specCore s) f
        res <- lam prims (coreFuncBody func)
        modify $ \s -> s{specCore = Map.insert f func{coreFuncBody = res} (specCore s)
                        ,specActive = Set.delete f (specActive s)
                        ,specDone = Set.insert f (specDone s)}

    -- now inline the function if required
    s <- get
    when (not $ Set.member f $ specDone s) $ put s{specMissed = Set.insert f (specMissed s)}

    let func = coreFuncMap (specCore s) f
        body = coreFuncBody func
    if isConLambda body then
        return $ uncurry coreLam $ coreInlineFuncLambda func xs
     else if isLambda body then
        let fresh = take (lamArity body) $ freeVars 'v' \\ concatMap collectAllVars xs
        in return $ CoreLam fresh $ CoreApp (CoreFun f) (xs ++ map CoreVar fresh)
     else
        return $ CoreApp (CoreFun f) xs

lam prims (CoreApp (CoreVar x) xs)  = liftM (CoreApp (CoreVar  x)) (mapM (lam prims) xs)
lam prims (CoreApp (CoreCon  x) xs) = liftM (CoreApp (CoreCon  x)) (mapM (lam prims) xs)

lam prims (CoreApp (CoreLam xs body) ys) =
        lam prims $ coreApp (coreLam (drop n xs) (replaceFreeVars (zip xs ys) body)) (drop n ys)
    where n = min (length xs) (length ys)

lam prims (CoreApp (CoreCase on alts) xs) = lam prims $ CoreCase on [(a, CoreApp b xs) | (a,b) <- alts]

lam prims (CoreApp (CoreApp x ys) zs) = lam prims $ CoreApp x (ys++zs)

lam prims (CoreApp (CoreLet bind x) xs)
    | null (map fst bind `intersect` vxs) = lam prims $ CoreLet bind (CoreApp x xs)
    | otherwise = lam prims $ CoreLet (zip fresh (map snd bind)) (CoreApp x2 xs)
        where
            x2 = replaceFreeVars (zip (map fst bind) (map CoreVar fresh)) x
            fresh = freeVars 'v' \\ (vl ++ vxs)
            vl = collectAllVars (CoreLet bind x)
            vxs = nub $ concatMap collectAllVars xs

lam prims (CoreLet binds x) = do
    rhs <- mapM (lam prims . snd) binds
    let (ho,fo) = partition (isHO . snd) (zip (map fst binds) rhs)
    x2 <- lam prims $ replaceFreeVars ho x
    return $ coreLet fo x2

lam prims (CoreCase on alts) = do
    on2 <- lam prims on
    case on2 of
        CoreApp (CoreCon c) xs ->
            lam prims $ head $ 
                 [replaceFreeVars (zip (map fromCoreVar xs2) xs) rhs
                    | (CoreApp (CoreCon c2) xs2, rhs) <- alts, c2 == c] ++
                 [replaceFreeVars [(lhs,on2)] rhs | (CoreVar lhs,rhs) <- alts]
        _ -> do
            rhs <- mapM (lam prims . snd) alts
            return $ CoreCase on2 (zip (map fst alts) rhs)

lam prims x | isCoreLam x || isCoreVar x || isCoreConst x = return x

lam prims x = error $ show x



dropDollar xs = if not (null nums) && not (null dol) then reverse (tail dol) else xs
    where (nums,dol) = span isDigit $ reverse xs


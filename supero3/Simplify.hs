
-- Historical note: I originally wrote using a Core language based on
-- Sem, which had the right kind of simplify operation.

module Simplify(simplify, simplifyProg) where

import Type
import Control.Arrow
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State


simplifyProg :: [(Var,Exp)] -> [(Var,Exp)]
simplifyProg = map (second simplify)



simplify :: Exp -> Exp
simplify = fromRoot . relabel . reduce . relabel . toRoot



---------------------------------------------------------------------
-- EXPRESSION ROOTS

data ExpRoot = ExpRoot [Var] Var (Map.Map Var Exp)


toRoot :: Exp -> ExpRoot
toRoot = f []
    where
        f vs (Lam _ v x) = f (vs++[v]) x
        f vs (Let _ xs v) = ExpRoot vs v (Map.fromList xs)
        f vs x = f vs $ Let noname [("_letvar",x)] "_letvar"


fromRoot :: ExpRoot -> Exp
fromRoot (ExpRoot vs v xs) = lams vs $ Let noname (Map.toList xs) v


---------------------------------------------------------------------
-- REDUCE

-- all the bound redexes must be fully simple
-- none of the expressions may have a root let statement

data SReduce = SReduce {reduceNew :: Map.Map Var Exp}

reduce :: ExpRoot -> ExpRoot
reduce (ExpRoot free root mp) = flip evalState (SReduce Map.empty) $ do
    mapM_ var $ Map.keys mp
    fmap (ExpRoot free root) $ gets reduceNew
    where
        simp :: Var -> Exp -> State SReduce Exp
        simp v x = do
            x <- redex x
            modify $ \s -> s{reduceNew = Map.insert v x $ reduceNew s}
            return x

        var :: Var -> State SReduce Exp
        var v = do
            new <- gets reduceNew
            case (Map.lookup v new, Map.lookup v mp) of
                (Just y, _) -> return y
                (_, Nothing) -> return $ Var noname v
                (_, Just x) -> simp v x

        redex :: Exp -> State SReduce Exp
        redex (Var _ v) = var v
        redex (Let _ vs x) = do
            mapM_ (uncurry simp) vs
            var x
        redex o@(App _ v y) = do
            x <- var v
            case x of
                Con n c xs -> return $ Con (incName n) c $ xs ++ [y]
                _ -> return o
        redex o@(Case _ v alts) = do
            x <- var v
            case x of
                Con{} -> return $ head $ concatMap (f x) alts
                _ -> return o
            where f (Con _ c vs) (Con _ c2 vs2, x) | c == c2 = [Let noname (("_letvar",x): zip vs2 (map (Var noname) vs)) "_letvar"]
                  f _ (Var{}, x) = [x]
                  f _ _ = []
        redex x = return x


---------------------------------------------------------------------
-- RELABEL

-- do a GC, variable normalisation, variable flattening

data SRelabel = SRelabel {relabelOld :: Map.Map Var Var, relabelNew :: Map.Map Var Exp, relabelVars :: [Var]}

relabel :: ExpRoot -> ExpRoot
relabel (ExpRoot free root mp) = flip evalState s0 $ do
    root2 <- move root
    old <- gets relabelOld
    free2 <- return [Map.findWithDefault "_" x old | x <- free]
    fmap (ExpRoot free2 root2) $ gets relabelNew
    where
        s0 = SRelabel Map.empty Map.empty ['v':show i | i <- [1..]]
        fresh = do v:vs <- gets relabelVars ; modify $ \s -> s{relabelVars=vs} ; return v
        freshN n = replicateM n fresh

        rename x y = modify $ \s -> s{relabelOld = Map.insert x y $ relabelOld s}
        record x y = modify $ \s -> s{relabelNew = Map.insert x y $ relabelNew s}

        move v = do
            old <- gets relabelOld
            case (Map.lookup v old, Map.lookup v mp) of
                (Just y, _) -> return y
                (_, Nothing) | v `elem` free -> do y <- fresh; rename v y; return y
                             | otherwise -> return v
                (_, Just (Var _ y)) -> move y
                (_, Just x) -> do
                    y <- fresh
                    rename v y
                    x <- f Map.empty x
                    record y x
                    return y

        f mp (App n x y) = liftM2 (App n) (var mp x) (var mp y)
        f mp (Case n x xs) = liftM2 (Case n) (var mp x) (mapM (alt mp) xs)
        f mp (Con n c xs) = liftM (Con n c) $ mapM (var mp) xs
        f mp (Var n x) = liftM (Var n) (var mp x)
        f mp (Let n vxs x) = do
            let (vs,xs) = unzip vxs
            vs2 <- replicateM (length vs) fresh
            xs2 <- mapM (f mp) xs
            x2 <- var (Map.fromList (zip vs vs2) `Map.union` mp) x
            return $ Let n (zip vs2 xs2) x2

        alt mp (Con n c vs, x) = do
            vs2 <- replicateM (length vs) fresh
            x2 <- f (Map.fromList (zip vs vs2) `Map.union` mp) x
            return (Con n c vs2, x2)
        alt mp (v, x) = fmap ((,) v) $ f mp x

        var mp v = case Map.lookup v mp of
            Nothing -> move v
            Just y -> return y

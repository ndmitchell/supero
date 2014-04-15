{-# LANGUAGE PatternGuards #-}

module Simplify(simplify, simplifyProg) where

import Util
import Type
import Control.Arrow
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Generics.Uniplate.Data
import Debug.Trace


simplifyProg :: [(Var,Exp)] -> [(Var,Exp)]
simplifyProg = inlineVars . map (second $ transform simplify)



inlineVars :: [(Var,Exp)] -> [(Var,Exp)]
inlineVars xs = [(v, subst sub e) | (v,e) <- xs]
    where sub = [(v,e) | (v,x) <- xs, Just e <- [f x]]
          f (App _ x []) = g x
          f (Let _ [] x) = g x
          f _ = Nothing
          g v = case lookup v xs of
                    Just x -> f x `mplus` Just v
                    Nothing -> Just v



-- We run relabel 3 times:
-- first time round gives unique names
-- then we simplify (assuming unique names)
-- second time round does GC
-- third time round does variable normalisation
simplify :: Exp -> Exp
simplify = fixEq simplify1

simplify1 x = f "v" $ f "_2" $ runFreshExp "_1" x $ skipLam reduce =<< relabel Map.empty x
    where f s x = runFreshExp s x $ relabel Map.empty x


runFreshExp :: String -> Exp -> Fresh a -> a
runFreshExp v x act = runFresh v $ filterFresh (`notElem` free x) >> act


skipLam :: (Exp -> Fresh Exp) -> Exp -> Fresh Exp
skipLam f (Lam n v x) = fmap (Lam n v) $ skipLam f x
skipLam f x = f x


---------------------------------------------------------------------
-- RELABEL

-- relabel, normalise the order of let bindings, and do a GC pass over unused let expressions
relabel :: Map.Map Var Var -> Exp -> Fresh Exp
relabel ren (Lam n v x) = do
    v2 <- fresh
    fmap (Lam n v2) $ relabel (Map.insert v v2 ren) x

-- let does the actual GC and reorder
relabel ren (Let n xs v) = do
    xs <- return $ rebind v xs
    vs2 <- freshN $ length xs
    ren <- return $ Map.fromList (zip (map fst xs) vs2) `Map.union` ren
    let v2 = relabelVar ren v
    xs2 <- mapM (f ren) (zip vs2 $ map snd xs)
    case xs2 of
        [(w2,y2)] | w2 == v2 -> return y2
        _ -> return $ Let n xs2 v2  
    where
        f ren (v,x) = fmap ((,) v) $ relabel ren x

relabel ren (Case n v xs) = do
    fmap (Case n $ relabelVar ren v) $ mapM (f ren) xs
    where
        f ren (Con n c vs,b) = do
            vs2 <- freshN $ length vs
            ren <- return $ Map.fromList (zip vs vs2) `Map.union` ren
            fmap ((,) $ Con n c vs2) $ relabel ren b
        f ren (App n v [],b) = do
            v2 <- fresh
            fmap ((,) $ App n v2 []) $ relabel (Map.insert v v2 ren) b

relabel ren (App n v1 v2) = return $ App n (relabelVar ren v1) (map (relabelVar ren) v2)
relabel ren (Con n c vs) = return $ Con n c $ map (relabelVar ren) vs

relabelVar ren v = Map.findWithDefault v v ren


-- put in order, and do a GC
rebind :: Var -> [(Var,Exp)] -> [(Var,Exp)]
rebind v xs | v `notElem` map fst xs = []
            | otherwise = [(a, fromJustNote "rebind" $ lookup a xs) | a <- order [] $ need [v]]
    where
        -- (a,b) means a relies on b
        pairs = [(a,b) | (a,b) <- xs, b <- free b, b `elem` map fst xs]

        need seen = if null next then seen else need (seen++next)
            where next = [b | (a,b) <- pairs, a `elem` seen, b `notElem` seen]

        order done [] = done
        order done todo = if null a then error "rebind circle" else order (done++a) b
            where (a,b) = partition f todo
                  f x = all (\(a,b) -> a /= x || b `elem` done) pairs


---------------------------------------------------------------------
-- REDUCE

-- reduce, assuming that let variables are sorted by order of use
-- i.e. will only depend on variables defined after you
reduce :: Exp -> Fresh Exp
reduce (Let n xs v) = do
    xs2 <- f [] xs
    return $ case lookup v xs2 of
        Just (App _ v2 []) -> Let n xs2 v2
        _ -> Let n xs2 v
    where
        f :: [(Var,Exp)] -> [(Var,Exp)] -> Fresh [(Var,Exp)]
        f done ((v,e):odo)
            | App n v2 [] <- e =
                let g xs = [(a,subst [(v,v2)] b) | (a,b) <- xs]
                in f ((v,e) : g done) (g odo) -- FIXME: This stage may not generate a fixed point!!!
            | Let n xs v2 <- e = f done (reverse xs ++ [(v,App n v2 [])] ++ odo)
            | App n v1 (v2:vs) <- e, Just e2@Lam{} <- lookup v1 done = do
                Lam _ v3 e3 <- relabel Map.empty e2
                v4 <- fresh
                f done ((v4, subst [(v3,v2)] e3):(v,App n v4 vs):odo)
            | App _ v1 v2 <- e, Just e2@(Con n c vs) <- lookup v1 done = do
                f done ((v,Con n c (vs++v2)):odo)
            | App _ v1 v2 <- e, Just e2@(App n v3 vs) <- lookup v1 done, Just a <- arity v3, a >= length (v2 ++ vs) =
                f done ((v,App n v3 (vs++v2)):odo)
            | Case n v2 alts <- e, Just (Con _ c vs) <- lookup v2 done =
                let pick (Con _ c2 vs2, x) | c == c2 = [subst (zip vs2 vs) x]
                    pick (App _ vm [], x) | c `notElem` smallCtors = [subst [(vm,v2)] x]
                    pick _ = []
                    r = head $ concatMap pick alts ++ error
                            ("confused, no case match...\n" ++ show n ++ "\n" ++ pretty (Con noname c vs) ++ "\n" ++ pretty e)
                in f done ((v,r):odo)
            | otherwise = f ((v,e):done) odo
        f done [] = return done


reduce x = return x



smallCtors = ["Nothing","Just","True","False","(:)","[]",":"]




{-

removeNestedLet :: Exp -> Exp
removeNestedLet 
    





data SSimp = SSimp {sFresh :: [Var], sBind :: Map.Map Var (Maybe Var, Maybe Exp)}

simp :: Exp -> Exp
simp x = flip evalState s0 $ do
        let FlatExp free bind v = toFlat x
        let free2 = take (length free) $ freshVars 'w'
        modify $ \s -> s{sBind = Map.insert [] (Nothing, Just $ Let bind v) $ Map.fromList [([a],(Just b,Nothing)) | (a,b) <- zip free free2]}
        v2 <- var []
        bind <- gets fromBind
        return $ fromFlat $ FlatExp (map snd ws) (sortOn fst [(a,b) | (Just a,Just b) <- Map.elems bind]) v2
    where
        s0 = SSimp (freshVars 'v') Map.empty

        -- bind a variable, find out where you put it
        var :: [Var] -> State SSimp Var
        var v = do
            s <- gets sBind
            case s Map.! v of
                (Just v2, _) -> return v2
                (Nothing, Just e) -> do
                    v2 <- fresh
                    e2 <- ren resolve e
                    op <- look
                    e3 <- return $ fuse look e2
                    modify $ \s -> s

                    case force of








-- rename a variable, calling the function on all unbound
-- and giving fresh names to all bound
ren :: FreshState s => (Var -> State s (Maybe Var)) -> Exp -> State s Exp
ren = undefined













simplify1 = fromNest . second renest . toNest

































data Nest = Nest [(Var, Nest)] Exp deriving Show



-- convert to the new representation
toNest :: Exp -> ([Var], Nest)
toNest (Lam _ v x) = (v:a,b)
    where (a,b) = toNest x
toNest x = ([], f x)
    where
        f (Let _ xs y) = Nest (map (second f) xs) (Var noname y)
        f x = Nest [] x



-- optimise the nested function
renest :: Nest -> Nest
renest x = x



data SFrom = SFrom {fromFresh :: [Var], fromBind :: Map.Map [Var] (Maybe Var, Maybe Exp)}

instance FreshState SFrom where
    getFresh = fromFresh
    setFresh x vs = x{fromFresh=vs}


-- convert back, doing the relabelling
-- should also drop all irrelevant ones
fromNest :: ([Var], Nest) -> Exp
fromNest (vs,x) = flip evalState (SFrom (freshVars 'v') Map.empty) $ do
        let ws = zip vs $ freshVars 'w'
        modify $ \s -> s{fromBind = Map.fromList [([a], (Just b, Nothing)) | (a,b) <- ws]}
        populate [] x
        r2 <- var []
        bind <- gets fromBind
        return $ fromFlat $ FlatExp (map snd ws) (sortOn fst [(a,b) | (Just a,Just b) <- Map.elems bind]) r2
    where
        populate :: [Var] -> Nest -> State SFrom ()
        populate v (Nest xs y) = do
            modify $ \s -> s{fromBind = Map.insert v (Nothing,Just y) $ fromBind s}
            sequence_ [populate (v++[a]) b | (a,b) <- xs]


        -- assign a new variable, or look up existing one
        var :: [Var] -> State SFrom Var
        var v = do
            s <- get
            let op = resolve (Map.keysSet $ fromBind s) v
            case Map.findWithDefault (error $ "don't know about variable: " ++ show v) v (fromBind s) of
                (Just v2, _) -> return v2
                (_, Just (Var _ v)) -> maybe (return v) var $ op v
                (_, Just e) -> do
                    v2 <- fresh
                    e <- exp op e
                    modify $ \s -> s{fromBind=Map.insert v (Just v2, Just e) $ fromBind s}
                    return v2


        -- process an expression, given a way to resolve variables
        exp :: (Var -> Maybe [Var]) -> Exp -> State SFrom Exp
        exp op = f Map.empty
            where
                f :: Map.Map Var Var -> Exp -> State SFrom Exp
                f _ x | trace (pretty x) False = undefined
                f ren (Var n v1) = liftM (Var n) (g ren v1)
                f ren (App n v1 v2) = liftM2 (App n) (g ren v1) (g ren v2)
                f ren (Con n c vs) = liftM (Con n c) (mapM (g ren) vs)
                f ren (Case n v1 as) = liftM2 (Case n) (g ren v1) (mapM (h ren) as)
                f ren (Let n vs v1) = do
                    vs2 <- freshN $ length vs
                    let ren2 = Map.fromList (zip (map fst vs) vs2) `Map.union` ren
                    bs <- sequence [liftM2 (,) (g ren2 a) (f ren2 b) | (a,b) <- vs]
                    liftM (Let n bs) (g ren v1)
                f ren (Lam n v x) = do
                    x2 <- f ren x
                    liftM (Lam n v) $ return x2
{-                    v2 <- fresh
                    res <- liftM (Lam n v2) $ f (Map.insert v v2 ren) x
                    -- res <- {- liftM (Lam n v2) -} (f ren {- (Map.insert v v2 ren) -} x)
                    return res -}
                f ren x = error $ "fromNest.exp: " ++ show x

                h :: Map.Map Var Var -> (Pat, Exp) -> State SFrom (Pat, Exp)
                h ren (Con n c vs,y) = do
                    vs2 <- freshN $ length vs
                    y <- f (Map.fromList (zip vs vs2) `Map.union` ren) y
                    return (Con n c vs2, y)
                
                g :: Map.Map Var Var -> Var -> State SFrom Var
                g ren v = case (Map.lookup v ren, op v) of
                    (Just v2, _) -> return v2
                    (_, Just v2) -> var v2
                    _ -> return v


        -- resolve a variable
        resolve :: Set.Set [Var] -> [Var] -> Var -> Maybe [Var]
        resolve set root x = listToMaybe [b | a <- reverse $ inits root, let b = a ++ [x], b `Set.member` set]

-}



{-


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

-}

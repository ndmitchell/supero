{-
We want to church encode everything but Int/Integer

Primitives such as EQ_W return Bool though, so we introduce:

bool_ x f t = case x of
                 True  -> t
                 False -> f
-}

module Church(church) where

import Yhc.Core


church :: Core -> Core
church core = core{coreDatas = [], coreFuncs = boolFunc : dataFuncs core ++ mapUnderCore f (coreFuncs core)}
    where
        f (CoreCase on alts) = expandCase core on alts
        f (CoreCon x) = CoreFun x
        f o@(CoreApp (CoreFun x) xs) = case corePrimMaybe x of
            Just x | last (primType x) == PrimBool -> CoreApp (CoreFun "bool_") [o]
            _ -> o
        f x = x


expandCase :: Core -> CoreExpr -> [(CoreExpr,CoreExpr)] -> CoreExpr
expandCase core on alts | isCoreFun root = CoreApp on (map f ctors)
                        | otherwise = CoreCase on alts
    where
        ctors = coreDataCtors $ coreCtorData core ctor
        ctor = fromCoreFun root
        root = fst $ fromCoreApp $ fst $ head alts

        f ctr = head $ [coreLam (map fromCoreVar args) rhs | (lhs,rhs) <- alts
                       , (CoreFun c,args) <- [fromCoreApp lhs], c == coreCtorName ctr] ++
                       [coreLam vrc (snd $ last alts)]
            where vrc = map (var 'c') [1..length (coreCtorFields ctr)]


boolFunc = CoreFunc "bool_" ["x","t","f"] $
    CoreCase (CoreVar "x")
        [(CoreCon "Prelude.True" , CoreVar "t")
        ,(CoreCon "Prelude.False", CoreVar "f")]


dataFuncs :: Core -> [CoreFunc]
dataFuncs = concatMap (ctorFuncs . coreDataCtors) . coreDatas


ctorFuncs :: [CoreCtor] -> [CoreFunc]
ctorFuncs cs = zipWith f [1..] cs
    where
        vrb = map (var 'b') [1..length cs]
    
        f n c = CoreFunc (coreCtorName c) (vra ++ vrb)
                         (CoreApp (CoreVar (var 'b' n)) (map CoreVar vra))
            where
                vra = map (var 'a') [1..length (coreCtorFields c)]

var c i = c : show i

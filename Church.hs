
module Church(church) where

import Yhc.Core


church :: Core -> Core
church core = core{coreDatas = [], coreFuncs = dataFuncs core ++ mapUnderCore f (coreFuncs core)}
    where
        f (CoreCase on alts) = expandCase core on alts
        f (CoreCon x) = CoreFun x
        f x = x


expandCase :: Core -> CoreExpr -> [(CoreExpr,CoreExpr)] -> CoreExpr
expandCase core on alts = CoreApp on (map f ctors)
    where
        ctors = coreDataCtors $ coreCtorData core ctor
        ctor = fromCoreFun $ fst $ fromCoreApp $ fst $ head alts

        f ctr = head $ [coreLam (map fromCoreVar args) rhs | (lhs,rhs) <- alts
                       , (CoreFun c,args) <- [fromCoreApp lhs], c == coreCtorName ctr] ++
                       [coreLam vrc (snd $ last alts)]
            where vrc = map (var 'c') [1..length (coreCtorFields ctr)]


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

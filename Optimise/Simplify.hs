
module Optimise.Simplify(simplify) where

import Yhc.Core
import Yhc.Core.FreeVar3
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map


simplify cont x@(CoreCase (CoreVar on) alts) | on `elem` collectFreeVars (CoreCase (CoreLit $ CoreInt 0) alts) =
        liftM (CoreCase (CoreVar on)) (mapM f alts)
    where
        f (pat@(PatCon c vs),rhs) = do
            let lhs = coreApp (CoreCon c) (map CoreVar vs)
            rhs <- transformM cont $ replaceFreeVars [(on,lhs)] rhs
            return (pat,rhs)

        f (lhs,rhs) = return (lhs,rhs)

simplify cont o@(CoreLet bind x) | not (null ctrs) && not (isCoreLetRec o) = do
        (newbinds,oldbinds) <- mapAndUnzipM f ctrs
        transformM cont $ coreLet (concat newbinds ++ other) $ replaceFreeVars oldbinds x
    where
        (ctrs,other) = partition (isCoreCon . fst . fromCoreApp . snd) bind

        f (name,x) = do
                vs <- replicateM (length tl) getVar
                return (zip vs tl, (name, coreApp hd (map CoreVar vs)))
            where (hd,tl) = fromCoreApp x

-- be careful with letrec
simplify cont o@(CoreLet bind x) | not (null lam) && not (isCoreLetRec o) = do
        x <- replaceFreeVarsUnique lam x
        transformM cont $ coreLet other x
    where
        (lam,other) = partition (isCoreLam . snd) bind

simplify cont (CoreApp (CoreFun x) [CoreLit (CoreInt a), CoreLit (CoreInt b)])
        | isJust p = cont $ CoreCon $ if fromJust p a b then "Prelude;True" else "Prelude;False"
    where
        p = Map.lookup x intPrims

simplify cont x = return x


intPrims :: Map.Map CoreFuncName (Int -> Int -> Bool)
intPrims = Map.fromList
    [("LT_W",(<))
    ,("GT_W",(>))
    ,("EQ_W",(==))
    ]


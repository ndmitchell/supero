
module Infer.Kind(inferKinds, Kind(..), Kinds) where

import Yhc.Core
import Control.Monad.State
import qualified Data.Map as Map


type Kinds = (Map.Map CoreFuncName Kind, Map.Map CoreVarName Kind)

data Kind = Star | Kind :-> Kind | DataFunc | Data
            deriving (Eq,Show)

{-

   C->
   
    |
    |
    
    C          ->
    
      \      /
       \    /
        
         *

-}

kinds = foldr1 (:->)


inferKinds :: Core -> Kinds
inferKinds core = f (Map.empty, Map.empty)
    where
        f k = let k2 = checkKinds core k
              in if k == k2 then k else f k2


checkKinds :: Core -> Kinds -> Kinds
checkKinds core k = execState (mapM_ (kindFunc core) $ coreFuncs core) k


kindFunc :: Core -> CoreFunc -> K ()
kindFunc core (CorePrim{}) = return ()
kindFunc core (CoreFunc name args body) = do
    bod <- kindExpr core Unknown body
    as <- mapM askVar args
    tellFunc (kinds (as ++ [bod])) name
    return ()


kindExpr :: Core -> Kind -> CoreExpr -> K Kind
kindExpr core k (CoreVar x) = tellVar k x

kindExpr core k (CoreFun x) = do
    r <- askFunc k x
    return $ combine x k r

kindExpr core k (CoreLet bind x) = do
    mapM_ f bind
    kindExpr k x
    where
        f (lhs,rhs) = do
            k <- askVar lhs
            k <- kindExpr k rhs
            tellVar k lhs

kindExpr core k (CoreCase on alts) = do
    kindExpr Data on
    xs <- mapM (kindExpr k . snd) alts
    return $ combines "case" xs

kindExpr core k (CoreApp x []) = kindExpr k x

kindExpr core k (CoreApp x [y]) =
    b <- kindExpr core Unknown y
    a :-> b <- kindExpr (Unknown :-> k) $ CoreApp x y1
    a <- kindExpr a y2
    return $ a :-> b

kindExpr core k (CoreApp x ys) =
    kindExpr core k (CoreApp (CoreApp x (init ys)) (last ys))
    
kindExpr core k (CoreCon x) = do
    let arity = length $ coreCtorFields $ coreCtor core x
    return $ combine x (kinds (replicate arity Unknown ++ [Data]) k

--- function stuff

type K a = State Kinds a

askVar = askWith snd
tellVar = tellWith (snd, \(a,_) b -> (a,b))
askFunc = askWith fst
tellFunc = tellWith (fst, \(_,b) a -> (a,b))


askWith :: (Kinds -> Map.Map String Kind) -> String -> K Kind
askWith sel x = return . Map.findWithDefault Unknown x . sel =<< get


tellWith :: (Kinds -> Map.Map String Kind, Kinds -> Map.Map String Kind -> Kinds) -> Kind -> String -> K Kind
tellWith (sel,rep) new x = do
    s <- get
    let old = Map.findWithDefault Unknown x (sel s)
        res = combine x new old
    put $ rep s $ Map.insert x res (sel s)
    return res


combine :: String -> Kind -> Kind -> Kind
combine name a b = error $ "Can't combine for: " ++ name ++ show (a,b)


combines name = foldr1 (combine name)




module Firstify.SpecState where

import Yhc.Core hiding (collectAllVars,uniqueBoundVarsCore)
import Yhc.Core.FreeVar3
import Yhc.Core.UniqueId
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace


type Spec a = State SpecState a

type FuncInfo = (Arity,CoreFunc)

data SpecState = SpecState
    {guess :: [Guess] -- information given when in Pending
    ,info :: Map.Map CoreFuncName FuncInfo -- information about functions
    ,pending :: Set.Set CoreFuncName -- those on the stack
    ,done :: Set.Set CoreFuncName -- those which have been done
    ,template :: Map.Map Template CoreFuncName -- templates created
    ,uid :: Int
    ,eid :: Int
    ,core :: Core
    ,specData :: Bool -- should you try and specialise data
    
    -- methods in here to break cycles
    ,localSpecExpr :: CoreExpr -> Spec CoreExpr
    }

-- given the function, and what you expected, do you get it
data Guess = Guess CoreFuncName String (FuncInfo -> Spec Bool)

instance Show Guess where
    show (Guess name val _) = name ++ "=" ++ val


instance UniqueId SpecState where
    getId = eid
    putId i x = x{eid=i}


-- the function being called, along with the arguments being passed
-- Nothing means the argument is simple (first-order)
data Template = TemplateApp  {templateName :: CoreFuncName, templateAppArgs :: [TempArg]}
              | TemplateCase {templateName :: CoreFuncName
                             ,templateCaseExtra :: Int
                             ,templateCaseAlts :: [(CoreCtorName, TempArg)]}
                deriving (Show,Eq,Ord)

-- an argument, and the number of extra variables it is given
data TempArg = TempApp CoreFuncName Int
             | TempNone
             | TempCon CoreCtorName [TempArg]
               deriving (Show,Eq,Ord)

isTempNone = (==) TempNone

-- map (+1) xs = Template "map" [Just (TemplateArg "+" 1), Nothing]
-- map id xs = Template "map" [Just (TemplateArg "id" 0), Nothing]

data Arity = Arity {arity :: Int, isData :: Bool}
             deriving (Show,Eq,Ord)


isSpecData :: Spec Bool
isSpecData = liftM specData get

isPending :: CoreFuncName -> Spec Bool
isPending name = get >>= return . Set.member name . pending

isDone :: CoreFuncName -> Spec Bool
isDone name = get >>= return . Set.member name . done

addPending :: CoreFuncName -> Spec ()
addPending name = modify $ \s -> s{pending = Set.insert name (pending s)}

delPending :: CoreFuncName -> Spec ()
delPending name = modify $ \s -> s{pending = Set.delete name (pending s)}

addDone :: CoreFuncName -> Spec ()
addDone name = modify $ \s -> s{done = Set.insert name (done s)}


retrieve :: (Eq i, Show i) => (FuncInfo -> Spec i) -> CoreFuncName -> Spec i
retrieve generate name = do
    pending <- isPending name
    if pending then do
        res <- answer
        let newguess = Guess name (show res) (liftM (== res) . generate)
        modify $ \s -> s{guess = newguess : guess s}
        return res
     else do
        done <- isDone name
        s <- get
        when (not done) $ specFunc name
        answer
    where
        answer = getFunc name >>= generate


specFunc :: CoreFuncName -> Spec ()
specFunc name = do
    -- () <- trace name $ return ()
    b1 <- isDone name
    b2 <- isPending name
    when (not b1 && not b2) $ do
        addPending name
        (_,func) <- getFunc name
        when (isCoreFunc func) $ do
            s <- get
            bod <- localSpecExpr s $ coreFuncBody func
            func <- return $ func{coreFuncBody=bod}
            arity <- calculateFuncArity func
            modify $ \s -> s{info = Map.insert name (arity,func) (info s)}
        delPending name
        addDone name


specMain :: Bool -> (CoreExpr -> Spec CoreExpr) -> Core -> Core
specMain specData coreExpr core_ = fromCoreFuncMap core $ Map.map snd $ info $ execState f s0
    where
        (core,eid) = runState (uniqueBoundVarsCore core_) 1
    
        fm = toCoreFuncMap core
        s0 = SpecState [] (Map.map g fm) Set.empty Set.empty Map.empty
                       (uniqueFuncsNext core) eid core specData coreExpr

        g x = (Arity (coreFuncArity x) False,x)

        f = do
            specFunc "main"
            b <- checkGuess
            when (not b) $ do
                () <- trace "A guess failed, retrying" $ return ()
                modify $ \x -> x{guess=[], pending=Set.empty, done=Set.empty}
                f

checkGuess :: Spec Bool
checkGuess = get >>= liftM and . mapM f . guess
    where
        f (Guess name value check) = do
            res <- getFunc name >>= check
            return res


getFunc :: CoreFuncName -> Spec FuncInfo
getFunc name = return . fromJust . Map.lookup name . info =<< get



-- more higher level, descion operations

getArity :: CoreFuncName -> Spec Arity
getArity = retrieve (return . fst)

isSaturated :: CoreFuncName -> [CoreExpr] -> Spec Bool
isSaturated name args = do
    Arity i _ <- getArity name
    return $ length args >= i


shouldInline :: CoreFuncName -> Spec Bool
shouldInline name = do
        (_,func) <- getFunc name
        res <- if isCoreFunc func then isCtor $ coreFuncBody func
                                  else return False
        return res
    where
        isCtor x@(CoreApp (CoreCon _) _) = isCtorApp x
        isCtor _ = return False

        isCtorApp (CoreApp (CoreCon x) xs) = liftM or $ mapM isCtorApp xs
        isCtorApp (CoreApp (CoreFun x) xs) = liftM not $ isSaturated x xs
        isCtorApp (CoreFun x) = isCtorApp (CoreApp (CoreFun x) [])
        isCtorApp _ = return False


exprArity :: CoreExpr -> Spec Arity
exprArity (CoreFun x) = exprArity (CoreApp (CoreFun x) [])
exprArity (CoreApp (CoreFun x) xs) = do
    Arity i d <- getArity x
    return $ Arity (i - length xs) d
exprArity (CoreCase on alts) = liftM maximum $ mapM (exprArity . snd) alts
exprArity (CoreLet bind x) = exprArity x
exprArity (CoreApp (CoreCon x) xs) = do
    c <- liftM core get
    let i = length $ coreCtorFields $ coreCtor c x
    return $ case i `compare` length xs of
        EQ -> Arity 0 True
        GT -> Arity (i - length xs) False
        _ -> Arity 0 False
exprArity _ = return $ Arity 0 False



calculateFuncArity :: CoreFunc -> Spec Arity
calculateFuncArity (CoreFunc name args body) = do
    Arity i d <- exprArity body
    return $ Arity (i + length args) d

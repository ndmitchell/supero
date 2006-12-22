
module Convert(convert) where

import Type
import Analysis
import Simplify

import Data.List
import Data.Maybe
import Control.Monad.State

import Debug.Trace

type Ask = CoreExpr
type DepAsk = ([Ask],Ask)


traced = False

traceMsg = traceIf traced

traceIf True msg x = trace msg x
traceIf False msg x = x


convert :: Core -> CoreEx
convert core = CoreEx $ f [] [([],mainApp)]
    where
        mainApp = CoreApp (CoreFun "main") [CoreVar ('v':show i) | i <- [1..length (coreFuncArgs main)]]
        main = coreFunc core "main"
        
        ares = analysis core
        
        f :: [Ask] -> [DepAsk] -> [CoreFuncEx]
        f done [] = []
        f done todo
            | now `elem` done = f done todo2
            | otherwise = case normaliseAsk ares (deps,now) of
                              Nothing -> func : f (now:done) (map (\a -> (now:deps,a)) addition ++ todo2)
                              Just extra -> f done (extra ++ todo2)
            where
                now@(CoreApp (CoreFun name) args) = snd (head todo)
                (match,todo2) = partition ((==) now . snd) todo
                deps = nub $ concatMap fst match
                
                (func,addition) = g now
        
        
        g :: Ask -> (CoreFuncEx, [Ask])
        g ask = (func, req)
            where
                func = createFunc core ares ask
                req = nub $ map normaliseFree $ collectAsk $ coreFuncExBody func



-- take an application to the body
createFunc :: Core -> Analysis -> Ask -> CoreFuncEx
createFunc core ares orig@(CoreApp (CoreFun name) args) = traceMsg ("specialise: " ++ show orig) $
        CoreFuncEx name (args ++ map CoreVar newargs) body2
    where
        (newargs,body) = coreInlineFuncLambda (coreFunc core name) args
        body2 = createBody core ares name body


createBody :: Core -> Analysis -> String -> CoreExpr -> CoreExpr
createBody core ares fname x = mapUnderCore remCorePos $ fixp $ newContext "" x
    where
        fixp x = if x2 == x3 then x2 else fixp x3
            where
                x2 = simplify core x
                x3 = mapUnderCore f x2
    
        f (CoreCase (CoreApp (CorePos ctx (CoreFun name)) args) alts)
                | name `notElem` words ctx && analysisInline ares name && null extra
                = traceMsg ("case-inline(" ++ fname ++ "): " ++ name) $ CoreCase uexpand alts
            where
                uexpand = uniqueFreeVarsWithout (concatMap collectAllVars sources) expand
                sources = expand : concat [[a,b] | (a,b) <- alts]
                
                (extra,expand) = coreInlineFuncLambda func{coreFuncBody = body} args
                body = newContext (name ++ " " ++ ctx) (coreFuncBody func)
                func = coreFunc core name
        
        f (CoreCase inner@(CorePos ctx (CoreFun name)) alts) = f (CoreCase (CoreApp inner []) alts)
        
        f x = x



newContext :: String -> CoreExpr -> CoreExpr
newContext msg = mapUnderCore $ \x -> case x of
    CoreFun x -> CorePos msg (CoreFun x)
    x -> x
        
        
addContext :: String -> CoreExpr -> CoreExpr
addContext msg = mapUnderCore $ \x -> case x of
    CorePos x y -> CorePos (msg ++ " " ++ x) y
    x -> x


-- decide which functions look useful
collectAsk :: CoreExpr -> [Ask]
collectAsk x = f x
    where
        f org@(CoreApp (CoreFun _) args) = org : concatMap f args
        f (CoreFun x) = [CoreApp (CoreFun x) []]
        f x = concatMap f $ getChildrenCore x


-- RULES
-- Return Nothing to say that the current normalisation is valid
--                means that there are no bad dependancies in there
-- Return Just xs to give a new list of dependancies

normaliseAsk :: Analysis -> DepAsk -> Maybe [DepAsk]
normaliseAsk ares (deps, orig@(CoreApp (CoreFun name) _)) =
        if null extra && res2 == orig then Nothing else Just $ (deps,res2) : extra
    where
        res2 = normaliseFree res
        (res,(_,extra)) = runState (mapUnderCoreM f orig) (freeVars 'v' \\ collectFreeVars orig, [])
        
        f (CoreApp (CoreFun name) args) = do
                args2 <- zipWithM g [0..] args
                return $ CoreApp (CoreFun name) args2
            where
                acc = analysisSpecialise ares name
                
                g n arg | n `notElem` acc = return arg
                        | otherwise = do
                            (s:ss,extra) <- get
                            put (ss, normAsk ares (deps,arg) ++ extra)
                            return (CoreVar s)
        
        f x = return x
        
        normAsk a b = fromMaybe [b] (normaliseAsk a b)

-- only ever reached by inner call inside normaliseAsk
-- otherwise an Ask is guaranteed to be a CoreApp (CoreFun ...)
normaliseAsk _ _ = Just []


freeVars :: Char -> [String]        
freeVars c = [c:show i | i <- [1..]]


-- make sure all functions with the same free variable layout
-- have the same free variable information
normaliseFree :: CoreExpr -> CoreExpr
normaliseFree x = x3
    where
        vars1 = collectFreeVars x
        x2 = replaceFreeVars (zip vars1 (map CoreVar $ freeVars 'w' \\ vars1)) x
        vars2 = collectFreeVars x2
        x3 = replaceFreeVars (zip vars2 (map CoreVar $ freeVars 'v')) x2

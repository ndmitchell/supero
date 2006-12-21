
module Convert(convert) where

import Type
import Analysis
import Simplify

import Data.List
import Data.Maybe
import Control.Monad.State

import Debug.Trace

type Ask = CoreExpr


traced = False

traceMsg msg x | traced = trace msg x
               | otherwise = x


convert :: Core -> CoreEx
convert core = CoreEx $ f [] (normaliseAsk ares mainApp)
    where
        mainApp = CoreApp (CoreFun "main") [CoreVar ('v':show i) | i <- [1..length (coreFuncArgs main)]]
        main = coreFunc core "main"
        
        ares = analysis core
        
        f :: [Ask] -> [Ask] -> [CoreFuncEx]
        f done [] = []
        f done (p@(CoreApp (CoreFun name) args):ending) = func : f (req++done) (req++ending)
            where
                func = createFunc core ares p
                req = nub (concatMap (normaliseAsk ares) (collectAsk $ coreFuncExBody func)) \\ done



-- take an application to the body
createFunc :: Core -> Analysis -> Ask -> CoreFuncEx
createFunc core ares orig@(CoreApp (CoreFun name) args) = traceMsg ("specialise: " ++ show orig) $
        CoreFuncEx name (args ++ map CoreVar newargs) body2
    where
        (newargs,body) = coreInlineFuncLambda (coreFunc core name) args
        body2 = createBody core ares body


createBody :: Core -> Analysis -> CoreExpr -> CoreExpr
createBody core ares x = fixp x
    where
        fixp x = if x2 == x3 then x2 else fixp x3
            where
                x2 = simplify core x
                x3 = mapUnderCore f x2
    
        f (CoreCase (CoreApp (CoreFun name) args) alts) | analysisInline ares name && null extra =
                traceMsg ("case-inline: " ++ name) $ CoreCase uexpand alts
            where
                uexpand = uniqueFreeVarsWithout (concatMap collectAllVars sources) expand
                sources = expand : concat [[a,b] | (a,b) <- alts]
                
                (extra,expand) = coreInlineFuncLambda (coreFunc core name) args
        
        f (CoreCase (CoreFun name) alts) = f (CoreCase (CoreApp (CoreFun name) []) alts)
        
        f x = x




-- decide which functions look useful
collectAsk :: CoreExpr -> [Ask]
collectAsk x = f x
    where
        f org@(CoreApp (CoreFun _) args) = org : concatMap f args
        f (CoreFun x) = [CoreApp (CoreFun x) []]
        f x = concatMap f $ getChildrenCore x



normaliseAsk :: Analysis -> Ask -> [Ask]
normaliseAsk ares orig@(CoreApp (CoreFun name) _) = normaliseFree res : extra
    where
        (res,(_,extra)) = runState (mapUnderCoreM f orig) (freeVars 'v' \\ collectFreeVars orig, [])
        
        f (CoreApp (CoreFun name) args) = do
                args2 <- zipWithM g [0..] args
                return $ CoreApp (CoreFun name) args2
            where
                acc = analysisSpecialise ares name
                
                g n arg | n `notElem` acc = return arg
                        | otherwise = do
                            (s:ss,extra) <- get
                            put (ss, normaliseAsk ares arg ++ extra)
                            return (CoreVar s)
        
        f x = return x

-- only ever reached by inner call inside normaliseAsk
-- otherwise an Ask is guaranteed to be a CoreApp (CoreFun ...)
normaliseAsk _ _ = []


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

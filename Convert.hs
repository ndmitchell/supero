
module Convert(convert) where

import Type
import Data.List
import Data.Maybe
import Control.Monad.State

type Ask = CoreExpr


convert :: Core -> CoreEx
convert core = CoreEx $ f [] (normaliseAsk analysis mainApp)
    where
        mainApp = CoreApp (CoreFun "main") [CoreVar ('v':show i) | i <- [1..length (coreFuncArgs main)]]
        main = coreFunc core "main"
        
        analysis = analyseCore core
        
        f :: [Ask] -> [Ask] -> [CoreFuncEx]
        f done [] = []
        f done (p@(CoreApp (CoreFun name) args):ending) = func : f (req++done) (req++ending)
            where
                func = createFunc core p
                req = nub (concatMap (normaliseAsk analysis) (collectAsk $ coreFuncExBody func)) \\ done




-- convert functions in a very basic manner
basicConvert :: CoreFunc -> CoreFuncEx
basicConvert (CoreFunc name args body) = CoreFuncEx name (map CoreVar args) body



-- take an application to the body
createFunc :: Core -> Ask -> CoreFuncEx
createFunc core (CoreApp (CoreFun name) args) = CoreFuncEx name (args ++ map CoreVar newargs) $ mapUnderCore (f 5) body
    where
        (newargs,body) = inlineFunc core name args
        
        -- may only recursively inline if case f x of => case g x of
        f n orig@(CoreCase (CoreApp (CoreFun name) args) alts) | null extra && n > 0 =
            f (n-1) $ mapUnderCore (f 0) $ CoreCase (uniqueExpr expand) alts
            where
                (extra,expand) = inlineFunc core name args
        
        f n (CoreCase (CoreFun x) alts) = f n (CoreCase (CoreApp (CoreFun x) []) alts)
        
        f n orig@(CoreApp (CoreCase _ _) _) = f n $ CoreCase on (map g alts)
            where
                CoreApp (CoreCase on alts) args = uniqueExpr orig
                g (lhs,rhs) = (lhs, f n $ CoreApp rhs args)
        
        f n (CoreCase (CoreCase on alts1) alts2) = f n $ CoreCase on (map g alts1)
            where
                g (lhs,rhs) = (h lhs, f n $ CoreCase (h rhs) alts2)
                    where
                        h x = replaceFreeVars (zip vs (map CoreVar vars)) x
                        vs = allCoreVar lhs
                        vars = freeVars 'v' \\ (collectAllVars lhs ++ collectAllVars rhs)
        
        f n (CoreCase (CoreLet bind on) alts) = f n $ CoreLet bind (f n $ CoreCase on alts)
        
        f n (CoreLet binds (CoreCase on alts1))
            | disjoint [i | CoreVar i <- allCore on] (map fst binds) = f n $ CoreCase on (map g alts1)
            where g (lhs,rhs) = (lhs,f n $ coreLet (filter ((`notElem` allCoreVar lhs) . fst) binds) $ f n rhs)
        
        f n (CoreCase (CoreCon con) alts) = f n $ CoreCase (CoreApp (CoreCon con) []) alts
        
        f n (CoreCase on@(CoreApp (CoreCon con) fields) alts)
                | not $ null matches = head matches
            where
                matches = mapMaybe g alts
        
                g (CoreCon x, rhs) | x == con = Just rhs
                g (CoreApp (CoreCon x) xs, rhs) | x == con = Just $ replaceFreeVars (zip (map fromCoreVar xs) fields) rhs
                g (CoreVar x,rhs) = Just $ replaceFreeVars [(x,on)] rhs
                g _ = Nothing

        f n (CoreApp (CoreApp x xs) ys) = f n $ CoreApp x (xs++ys)

        f n x = x



uniqueExpr :: CoreExpr -> CoreExpr
uniqueExpr x = uniqueFreeVarsWithout (collectAllVars x) x



-- decide which functions look useful
collectAsk :: CoreExpr -> [Ask]
collectAsk x = f x
    where
        f org@(CoreApp (CoreFun _) args) = org : concatMap f args
        f (CoreFun x) = [CoreApp (CoreFun x) []]
        f x = concatMap f $ getChildrenCore x



type Analysis = [(String,[Int])] -- accumulators


normaliseAsk :: Analysis -> Ask -> [Ask]
normaliseAsk accs orig@(CoreApp (CoreFun name) _) = normaliseFree res : extra
    where
        (res,(_,extra)) = runState (mapUnderCoreM f orig) (freeVars 'v' \\ collectFreeVars orig, [])
        
        f (CoreApp (CoreFun name) args) = do
                args2 <- zipWithM g [0..] args
                return $ CoreApp (CoreFun name) args2
            where
                acc = fromMaybe [] (lookup name accs)
                
                g n arg | n `notElem` acc = return arg
                        | otherwise = do
                            (s:ss,extra) <- get
                            put (ss, normaliseAsk accs arg ++ extra)
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




analyseCore :: Core -> Analysis
analyseCore core = accs
    where
        accs = [("foldl",[1]),("iterate",[1]),("showIntAtBase",[4])
               ,("Prelude.Prelude.1054.showPosInt",[1]),("Prelude.Prelude.877.walk",[1])
               ,("Prelude.Prelude.1055.showPosInt",[1])
               ,("Prelude.Enum.Prelude.Integer.enumFrom",[0])
               ,("Prelude.Enum.Prelude.Integer.enumFromThen",[0,1])
               ,("Prelude.Enum.Prelude.Integer.toEnum",[0])
               ,("<=",[1]),("_enumFromToIncC",[1]),("_enumFromToDecC",[1])
               ,("Prelude.Enum.Prelude.Int.enumFrom",[0])
               ,("Prelude.Enum.Prelude.Int.enumFromThen",[0,1])
               ]



-- if you oversaturate, pass the extra arguments as an Apply
-- if you undersaturate, return the extra arguments as the first of the tuple
inlineFunc :: Core -> String -> [CoreExpr] -> ([String], CoreExpr)
inlineFunc core name args
        | null underArgs = ([], coreApp (replaceFreeVars (zip params args) body2) overArgs)
        | otherwise = (underArgs, replaceFreeVars (zip params (args ++ map CoreVar underArgs)) body2)
    where
        overArgs = drop nparams args
        underArgs = drop nargs params
    
        nargs = length args
        nparams = length params
        
    
        body2 = uniqueFreeVarsWithout (params ++ concatMap collectAllVars args) body
        CoreFunc _ params body = coreFunc core name


module Convert(convert) where

import Type
import Data.List
import Data.Maybe

type Ask = CoreExpr


convert :: Core -> CoreEx
convert core = CoreEx $ prims ++ f [] (normaliseAsk analysis mainApp)
    where
        mainApp = CoreApp (CoreFun "main") [CoreVar ('v':show i) | i <- [1..length (coreFuncArgs main)]]
        main = coreFunc core "main"
        
        prims = [CoreFuncEx name (map CoreVar args) body | CoreFunc name args body <- coreFuncs core, isPrimitive body]
        
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
createFunc core (CoreApp (CoreFun name) args) = CoreFuncEx name args $ mapUnderCore (f 5) body
    where
        (newargs,body) = inlineFunc core name args
        
        -- may only recursively inline if case f x of => case g x of
        f n orig@(CoreCase (CoreApp (CoreFun name) args) alts) | n > 0 =
            f (n-1) $ mapUnderCore (f 0) $ CoreCase expand alts
            where
                ([],expand) = inlineFunc core name args
                expand2 = uniqueFreeVarsWithout (collectAllVars expand) expand
        
        f n (CoreCase (CoreCase on alts1) alts2) = f n $ CoreCase on (map g alts1)
            where g (lhs,rhs) = (lhs, f n $ CoreCase rhs alts2)
        
        f n (CoreCase on@(CoreApp (CoreCon con) fields) alts) | not $ null matches = head matches
            where
                matches = mapMaybe g alts
                
                g (CoreApp (CoreCon x) xs, rhs) | x == con = Just $ replaceFreeVars (zip (map fromCoreVar xs) fields) rhs
                g (CoreVar x,rhs) = Just $ replaceFreeVars [(x,on)] rhs
                g _ = Nothing

        f n (CoreApp (CoreApp x xs) ys) = f n $ CoreApp x (xs++ys)

        f n x = x


-- decide which functions look useful
collectAsk :: CoreExpr -> [Ask]
collectAsk x = [y | y@(CoreApp (CoreFun _) _) <- allCore x]



type Analysis = ([String] -- primitive functions
                ,[(String,[Int])] -- accumulators
                )


normaliseAsk :: Analysis -> Ask -> [Ask]
normaliseAsk (prims,accs) x@(CoreApp (CoreFun name) _) = [x3 | name `notElem` prims]
    where
        vars1 = collectFreeVars x
        x2 = replaceFreeVars (zip vars1 (map CoreVar $ varsWith 'w' \\ vars1)) x
        vars2 = collectFreeVars x2
        x3 = replaceFreeVars (zip vars2 (map CoreVar $ varsWith 'v')) x2

        varsWith c = [c:show i | i <- [1..]]


analyseCore :: Core -> Analysis
analyseCore core = ([coreFuncName x | x <- coreFuncs core,  isPrimitive $ coreFuncBody x], [])





-- if you oversaturate, pass the extra arguments as an Apply
-- if you undersaturate, return the extra arguments as the first of the tuple
inlineFunc :: Core -> String -> [CoreExpr] -> ([String], CoreExpr)
inlineFunc core name args
        | null underArgs = ([], coreApp (replaceFreeVars (zip params args) body2) overArgs)
        | otherwise = error "Convert.inlineFunc, todo"
    where
        overArgs = drop nparams args
        underArgs = drop nargs params
    
        nargs = length args
        nparams = length params
        
    
        body2 = uniqueFreeVarsWithout (params ++ concatMap collectAllVars args) body
        CoreFunc _ params body = coreFunc core name

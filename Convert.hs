
module Convert(convert) where

import Type
import Data.List
import Data.Maybe

type Ask = CoreExpr


convert :: Core -> CoreEx
convert core = CoreEx $ f [] [normaliseAsk mainApp]
    where
        mainApp = CoreApp (CoreFun "main") [CoreVar ('v':show i) | i <- [1..length (coreFuncArgs main)]]
        main = coreFunc core "main"
        
        
        f :: [Ask] -> [Ask] -> [CoreFuncEx]
        f done [] = []
        f done (p@(CoreApp (CoreFun name) args):ending) = func : f (req++done) req
            where
                func = CoreFuncEx name args body
                body = createFunc core p
                req = map normaliseAsk (collectAsk body) \\ done




-- convert functions in a very basic manner
basicConvert :: CoreFunc -> CoreFuncEx
basicConvert (CoreFunc name args body) = CoreFuncEx name (map CoreVar args) body



-- take an application to the body
createFunc :: Core -> Ask -> CoreExpr
createFunc core (CoreApp (CoreFun name) args) = mapUnderCore (f 5) body
    where
        body = inlineFunc core name args
        
        f n orig@(CoreCase (CoreApp (CoreFun name) args) alts) | n > 0 =
            mapUnderCore (f (n-1)) $ CoreCase expand alts
            where
                expand = inlineFunc core name args
                expand2 = uniqueFreeVarsWithout (collectAllVars expand) expand
        
        f n (CoreCase (CoreCase on alts1) alts2) = f n $ CoreCase on (map g alts1)
            where g (lhs,rhs) = (lhs, f n $ CoreCase rhs alts2)
        
        f n (CoreCase on@(CoreApp (CoreCon con) fields) alts) | not $ null matches = head matches
            where
                matches = mapMaybe g alts
                
                g (CoreApp (CoreCon x) xs, rhs) | x == con = Just $ replaceFreeVars (zip (map fromCoreVar xs) fields) rhs
                g (CoreVar x,rhs) = Just $ replaceFreeVars [(x,on)] rhs
                g _ = Nothing

        f n x = x


-- decide which functions look useful
collectAsk :: CoreExpr -> [Ask]
collectAsk x = [y | y@(CoreApp (CoreFun _) _) <- allCore x]


normaliseAsk :: Ask -> Ask
normaliseAsk x = x3
    where
        vars1 = collectFreeVars x
        x2 = replaceFreeVars (zip vars1 (map CoreVar $ varsWith 'w' \\ vars1)) x
        vars2 = collectFreeVars x2
        x3 = replaceFreeVars (zip vars2 (map CoreVar $ varsWith 'v')) x2

        varsWith c = [c:show i | i <- [1..]]


inlineFunc :: Core -> String -> [CoreExpr] -> CoreExpr
inlineFunc core name args = replaceFreeVars (zip params args) body2
    where
        body2 = uniqueFreeVarsWithout (params ++ concatMap collectAllVars args) body
        CoreFunc _ params body = coreFunc core name

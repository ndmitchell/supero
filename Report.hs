
module Report(report) where

import Yhc.Core
import Yhc.Core.Play2
import qualified Data.Map as Map
import Data.Maybe


-- report every occurence of a non-saturated application
report :: Core -> [String]
report core = ["In " ++ name ++ ": " ++ x ++ " want:" ++ show arity ++ ", got:" ++ show app
        | CoreFunc name _ bod <- coreFuncs $ alwaysAppFun core
        , CoreApp (CoreFun x) xs <- everythingCore bod
        , let arity = (fromJust $ Map.lookup x table) :: Int
        , let app = length xs
        , arity /= app
        ]
    where
        table = Map.fromList [(coreFuncName x, coreFuncArity x) | x <- coreFuncs core]

alwaysAppFun :: Core -> Core
alwaysAppFun = traverseCore f
    where
        f (CoreFun x) = CoreApp (CoreFun x) []
        f (CoreApp (CoreApp x xs) ys) = CoreApp x (xs++ys)
        f x = x


module Firstify2.Terminate(weakenTemplate) where

import Firstify2.SpecState
import Data.List
import Debug.Trace
import Control.Monad.State
import qualified Data.Map as Map


weakenTemplate :: Template -> Spec (Maybe Template)
weakenTemplate (TemplateApp n xs@(_:_)) | name `isPrefixOf` n = return $ Just $ TemplateApp n xs2
    where
        name = "Prelude.Prelude.Prelude.1107.showPosInt"
        xs2 = init xs ++ [TempNone]

weakenTemplate x = do
    s <- get
    if uid s > 1000 then
        error $ unlines $ map showTemplate $ Map.toList $ template s
     else
        return $ Just x


showTemplate (t, y) = y ++ " = " ++ f1 t
    where
        f1 (TemplateApp name args) = unwords $ name : map f2 args
        f1 (TemplateCase name extra alts) = "case " ++ f2 (TempApp name extra) ++ " of " ++ concatMap f3 alts
        
        f2 (TempApp name extra) = name ++ " #" ++ show extra
        f2 TempNone = "_"
        f2 (TempCon c args) = "(" ++ unwords (c : map f2 args) ++ ")"

        f3 ("",x) = "_ -> " ++ f2 x ++ "; "
        f3 (c ,x) = c ++ " -> " ++ f2 x ++ "; "

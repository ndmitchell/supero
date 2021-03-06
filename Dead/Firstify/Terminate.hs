
module Firstify.Terminate(weakenTemplate) where

import Firstify.SpecState
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
        let mp = Map.fromList $ map (\(a,b) -> (b,a)) $ Map.toList $ template s in
        error $ unlines $ map (showTemplate mp) $ Map.toList $ template s
     else
        return $ Just x


showTemplate mp (t, y) = y ++ " = " ++ f1 t
    where
        f1 (TemplateApp name args) = unwords $ g name : map f2 args
        f1 (TemplateCase name extra alts) = "case " ++ f2 (TempApp name extra) ++ " of " ++ concatMap f3 alts
        
        f2 (TempApp name extra) = g name ++ " #" ++ show extra
        f2 TempNone = "_"
        f2 (TempCon c args) = "(" ++ unwords (c : map f2 args) ++ ")"

        f3 ("",x) = "_ -> " ++ f2 x ++ "; "
        f3 (c ,x) = c ++ " -> " ++ f2 x ++ "; "

        g x = case Map.lookup x mp of
                  Nothing -> x
                  Just y -> "<" ++ f1 y ++ ">"


-- This module should be added to Yhc.Core

module Unique(uniqueFuncs, uniqueName) where

import Yhc.Core.Type
import Yhc.Core.Play2

import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map


-- rule 
uniqueFuncs :: Core -> Core
uniqueFuncs core = core{coreFuncs = zipWith f names (coreFuncs core)}
    where
        f new func = func{coreFuncName=new, coreFuncBody=traverseCore g $ coreFuncBody func}
        g (CoreFun x) = CoreFun $ rep x
        g x = x

        (names,rep) = uniqueNames (map coreFuncName $ coreFuncs core)


uniqueName :: String -> Int -> String
uniqueName name uid = dropNum name ++ show uid
    where dropNum = reverse . dropWhile isDigit . reverse


-- make sure that the mapping is an isomorphism, and that
-- each item in the result set does not end with a digit
-- add _'s are needed to make the functions newly unique
uniqueNames :: [String] -> ([String], String -> String)
uniqueNames xs = (new, \x -> fromJust $ Map.lookup x (Map.fromList $ zip xs new))
    where
        new = f Set.empty xs

        f seen [] = []
        f seen (x:xs)
            | null x = x : f seen xs
            | otherwise = g (x ++ ['_' | isDigit $ last x]) seen xs
            
        g add seen xs
            | add `Set.member` seen = g (add ++ "_") seen xs
            | otherwise = add : f (Set.insert add seen) xs

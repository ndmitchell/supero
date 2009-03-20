
module Compiler.Expr where

import Data.Maybe

type Name = String

-- Invariant: No duplicate names
type Prog e = [(Name,e)]

-- Invariant: Must not mention any function names in the first part
-- Second list of names are suggested names (may be "")
type Residual e = (e, [(Name,e)])

class (Show e, Eq e) => Expr e where
    step :: Prog e -> e -> Either (Residual e) [e]
    (<<|) :: [e] -> e -> Maybe (Residual e)
    residual :: e -> [Name] -> e


resolve :: Prog e -> Name -> e
resolve xs x = fromMaybe (error $ unwords $ "Couldn't resolve" : x : "in" : map fst xs) $ lookup x xs

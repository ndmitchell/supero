
module Compiler.Expr where

import Data.Maybe

type Name = String


type Call e = (Name, [String])

-- First part is the list of expressions to supercompile
-- Second list is how to put them back together
type Residual e = ([e], [Call e] -> e)

-- i is some precomputed information about a source function
-- e is an expression in the src or dest language
class (Show e, Eq e) => Expr i e where
    step :: (Name -> (i,e)) -> e -> Either (Residual e) [e]
    (<<|) :: [e] -> e -> Maybe (Residual e)
    call :: Name -> [String] -> e -- to construct expressions in the result program


resolve :: Prog e -> Name -> e
resolve xs x = fromMaybe (error $ unwords $ "Couldn't resolve" : x : "in" : map fst xs) $ lookup x xs



name :: e -> (Name, [e])
call :: Name -> [String] -> e

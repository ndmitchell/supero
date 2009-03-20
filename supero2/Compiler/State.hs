
module Compiler.State where

import Compiler.Expr
import Control.Monad.State

data S e = S {names :: [(e,Name)], history :: [e], result :: Prog e}
s0 = S [] [] []

type SS e a = State (S e) a


getName :: Eq e => Name -> e -> SS e Name
getName hint e = do
    names <- gets names
    case lookup e names of
        Just v -> return v
        Nothing -> do
            let r = hint ++ "_" ++ show (length names)
            modify $ \s -> s{names = (e,r):names}
            return r

getHistory :: SS e [e]
getHistory = gets history

addHistory :: e -> SS e ()
addHistory e = modify $ \s -> s{history = e : history s}

addResult :: Name -> e -> SS e ()
addResult name e = modify $ \s -> s{result = (name,e) : filter ((/=) name . fst) (result s)}

hasResult :: Name -> SS e Bool
hasResult name = do r <- gets result; return $ name `elem` map fst r

run :: SS e () -> Prog e
run x = result $ execState x s0



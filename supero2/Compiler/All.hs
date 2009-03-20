
module Compiler.All where

import Compiler.Expr
import Compiler.State
import Data.Maybe
import Control.Monad
import Debug.Trace


compile :: Expr e => Prog e -> Prog e
compile prog = run $ addWith "main" (resolve prog "main")
    where
        add (hint,x) = do
            name <- getName hint x
            b <- hasResult name
            unless b $ addWith name x
            return name

        addWith name x = do
            addResult name x -- dummy to reserve a slot
            history <- getHistory
            (r,rs) <- case history <<| x of
                Nothing -> addHistory x >> return (expr prog x)
                Just x -> return x
            xs <- mapM add rs
            addResult name $ residual r xs


expr :: Expr e => Prog e -> e -> Residual e
expr prog = f []
    where
        f seen x = case step prog x of
            _ | length seen > 2 -> error $ show x
            Left x -> x
            Right [] -> error "step invariant violated"
            Right xs@(x:_) | all (isJust . snd) rs -> head [r | (_,Just r) <- rs]
                           | otherwise -> f (x2:seen) x2
                where rs = zip xs $ map (seen <<|) xs
                      x2 = head [x | (x,Nothing) <- rs]

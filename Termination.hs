
module Termination where

import Yhc.Core
import Yhc.Core.UniqueId
import Yhc.Core.FreeVar3
import Control.Monad
import CoreUtil


type Subst = [(CoreVarName,CoreExpr)]

-- | Most specific generalisation
--   I tried to understand the book, failed, so guessed instead.
msg :: UniqueIdM m => CoreExpr -> CoreExpr -> m (CoreExpr, Subst, Subst)
msg x y = lca x y


-- | Homeomorphic embedding
(<<|) :: CoreExpr -> CoreExpr -> Bool
CoreLit _ <<| CoreLit _ = True -- can omit as I don't do maths?
CoreVar _ <<| CoreVar _ = True
x <<| y =
    any (x <<|) (children y) ||
    (x `eq1` y && and (zipWith (<<|) (children x) (children y)))



-- | Least common anti-instance
lca :: UniqueIdM m => CoreExpr -> CoreExpr -> m (CoreExpr, Subst, Subst)
lca x y | x `eq1` y = do
        let rep = snd $ uniplate x
        (cs,sx,sy) <- liftM unzip3 $ zipWithM lca (children x) (children y)
        return (rep cs, concat sx, concat sy)
lca x y = do
    v <- getVar
    return (CoreVar v, [(v,x)], [(v,y)])

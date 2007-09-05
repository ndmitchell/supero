
module Termination where

import Yhc.Core
import CoreUtil


type Subst = [(CoreVarName,CoreExpr)]

msg :: CoreExpr -> CoreExpr -> (CoreExpr, Subst, Subst)
msg = undefined



(<<|) :: CoreExpr -> CoreExpr -> Bool
-- rule ommitted as I don't do Math's operations
-- CoreLit x <<| CoreLit y = True
CoreVar _ <<| CoreVar _ = True
x <<| y =
    any (x <<|) (children y) ||
    (x `eq1` y && and (zipWith (<<|) (children x) (children y)))

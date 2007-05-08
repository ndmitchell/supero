
module Firstify2.SpecExpr(specExpr) where

import Yhc.Core
import Firstify2.Spec


specExpr :: CoreExpr -> Spec CoreExpr
specExpr x = return x

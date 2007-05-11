
module Firstify2.Firstify where

import Yhc.Core
import Unique
import Firstify2.Spec
import Firstify2.SpecExpr


firstify :: Core -> Core
firstify core = coreReachable ["main"] $ fromCoreFuncMap core $ specMain specExpr $ toCoreFuncMap $ uniqueFuncs core


module Firstify2.Firstify where

import Yhc.Core
import Unique
import Firstify2.SpecExpr
import Firstify2.SpecState


firstify :: Core -> Core
firstify core = coreReachable ["main"] $ fromCoreFuncMap core $ specMain specExpr $ toCoreFuncMap $ uniqueFuncs core

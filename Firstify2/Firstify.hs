
module Firstify2.Firstify where

import Yhc.Core
import Unique
import Firstify2.SpecExpr
import Firstify2.SpecState


firstify :: Core -> Core
firstify = coreReachable ["main"] . specMain False specExpr


firstifyData :: Core -> Core
firstifyData = coreReachable ["main"] . specMain True specExpr



firstifyDataPrepare :: Core -> Core
firstifyDataPrepare = id

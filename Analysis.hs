
module Analysis(Analysis, analysis, analysisInline, analysisSpecialise) where

import Yhc.Core
import qualified Data.Map as Map
import qualified Data.Set as Set


data Analysis = Analysis (Set.Set String) (Map.Map String [Int])


-- | Can a function be inlined inside a case statement
analysisInline :: Analysis -> String -> Bool
analysisInline (Analysis x _) n = not $ Set.member n x


-- | Return the 0-based indexes of elements on which you can't specialise
analysisSpecialise :: Analysis -> String -> [Int]
analysisSpecialise (Analysis _ x) n = Map.findWithDefault [] n x


analysis :: Core -> Analysis
analysis core = Analysis (Set.fromList recursers) (Map.fromList accumulators)



recursers = ["foldl","Prelude.Prelude.1728.primBind2","Prelude.Prelude.1718.primBind1","report","dif","subset","dropWhile","foldr"]


accumulators :: [(String,[Int])]
accumulators = [("foldl",[1]),("iterate",[1]),("showIntAtBase",[4])
               ,("Prelude.Prelude.1054.showPosInt",[1]),("Prelude.Prelude.877.walk",[1])
               ,("Prelude.Prelude.1055.showPosInt",[1])
               ,("Prelude.Prelude.1058.showPosInt",[1])
               ,("Prelude.Prelude.1059.showPosInt",[1])
               ,("Prelude.Enum.Prelude.Integer.enumFrom",[0])
               ,("Prelude.Enum.Prelude.Integer.enumFromThen",[0,1])
               ,("Prelude.Enum.Prelude.Integer.toEnum",[0])
               ,("<=",[1]),("_enumFromToIncC",[1]),("_enumFromToDecC",[1])
               ,("Prelude.Enum.Prelude.Int.enumFrom",[0])
               ,("Prelude.Enum.Prelude.Int.enumFromThen",[0,1])
               ]

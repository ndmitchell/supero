
module Data.Homeomorphic.Check where

import qualified Data.Homeomorphic.Simple as H1
import qualified Data.Homeomorphic.Neil   as H2
import Data.Homeomorphic.Internal

data Homeomorphic k v = Homeomorphic (H1.Homeomorphic k v) (H2.Homeomorphic k v)

empty :: Homeomorphic k v
empty = Homeomorphic H1.empty H2.empty


insert :: Ord k => Shell k -> v -> Homeomorphic k v -> Homeomorphic k v
insert k v (Homeomorphic h1 h2) = Homeomorphic (H1.insert k v h1) (H2.insert k v h2)


-- additional Eq v constraint required to check the answers
-- additional Show constraints required to give a meainingful error message
find :: (Ord k, Eq v, Show v, Show k) => Shell k -> Homeomorphic k v -> [v]
find k (Homeomorphic h1 h2) =
        if r1 == r2 then r1 else
           error $ "Data.Homeomorhpic.Check.find: mismatch\n" ++
                   "Given: " ++ show k ++ "\n" ++
                   "H1: " ++ show r1 ++ "\n" ++
                   "H2: " ++ show r2 ++ "\n"
    where
        r1 = H1.find k h1
        r2 = H2.find k h2

findOne :: (Ord k, Eq v, Show v, Show k) => Shell k -> Homeomorphic k v -> Maybe v
findOne k h@(Homeomorphic h1 h2) = error "Data.Homeomorphic.Check.findOne: todo"

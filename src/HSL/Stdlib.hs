
module HSL.Stdlib (groupOn, sortOn, sortOnR, hist, tabs, tally) where


import           Data.Ord (comparing)
import           Data.List (groupBy, sortBy)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Tuple (swap)

import           HSL.Types


groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\a b -> f a == f b)


sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f = sortBy (comparing f)


sortOnR :: (Ord b) => (a -> b) -> [a] -> [a]
sortOnR f = sortBy (flip $ comparing f)


tally :: (Ord a, Num b) => [(a, b)] -> [(a, b)]
tally = Map.toAscList . foldl inc Map.empty
  where inc = (\m (k, v) -> Map.insertWith (+) k v m)


hist :: (Ord a) => [a] -> [(a, Int)]
hist = tally . flip zip (repeat 1)


tabs :: Datum a => a -> [T.Text] -> [a]
tabs _ = map (parseMany . T.split (=='\t'))


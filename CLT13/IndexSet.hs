{-# LANGUAGE TupleSections #-}

module CLT13.IndexSet where

import qualified Data.Map.Strict as M

type Power    = Int
type Index    = Int
type IndexSet = M.Map Index Power

indexUnion :: IndexSet -> IndexSet -> IndexSet
indexUnion = M.unionWith (+)

indexUnions :: [IndexSet] -> IndexSet
indexUnions = M.unionsWith (+)

-- Return the symbols from A that aren't in B, and their difference in Power.
-- Assumes that b is always smaller power than a
indexDiff :: IndexSet -> IndexSet -> IndexSet
indexDiff = M.differenceWith (\a b -> let c = a - b in if c <= 0 then Nothing else Just c)

-- constructors for IndexSet from Index
pow :: [Index] -> Power -> IndexSet
pow ixs p = M.fromList $ map (,p) ixs

pow1 :: [Index] -> IndexSet
pow1 = flip pow 1

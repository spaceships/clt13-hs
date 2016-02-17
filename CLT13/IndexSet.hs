{-# LANGUAGE TupleSections #-}

module CLT13.IndexSet where

import Control.Parallel.Strategies (NFData)
import qualified Data.Map.Strict as M

type Power    = Int
type Index    = Int
type IndexSet = M.Map Index Power

indexUnion :: IndexSet -> IndexSet -> IndexSet
indexUnion = M.unionWith (+)

indexUnions :: [IndexSet] -> IndexSet
indexUnions = M.unionsWith (+)

-- constructors for IndexSet from Index
pow :: [Index] -> Power -> IndexSet
pow ixs p = M.fromList $ map (,p) ixs

pow1 :: [Index] -> IndexSet
pow1 = flip pow 1

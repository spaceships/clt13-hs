{-# LANGUAGE TupleSections #-}

module CLT13.IndexSet where

import qualified Data.Map as M

type Power    = Int
type Index    = Int
type IndexSet = M.Map Int Power

indexUnion :: IndexSet -> IndexSet -> IndexSet
indexUnion = M.unionWith (+)

indexUnions :: [IndexSet] -> IndexSet
indexUnions = M.unionsWith (+)

pow :: [Index] -> Power -> IndexSet
pow ixs p = M.fromList $ map (,p) ixs

pow1 :: [Index] -> IndexSet
pow1 = flip pow 1

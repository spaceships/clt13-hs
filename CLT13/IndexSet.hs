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

indexEq :: IndexSet -> IndexSet -> Bool
indexEq x y = M.null (indexSub x y) && M.null (indexSub y x)

indexNeq :: IndexSet -> IndexSet -> Bool
indexNeq x y = not (indexEq x y)

indexSub :: IndexSet -> IndexSet -> IndexSet
indexSub a b = M.filter (/= 0) $ M.differenceWith subMaybe a b
  where
    subMaybe x y = let z = x - y in if z <= 0 then Nothing else Just z

-- constructors for IndexSet from Index
pow :: [Index] -> Power -> IndexSet
pow ixs p = M.fromList $ map (,p) ixs

pow1 :: [Index] -> IndexSet
pow1 = flip pow 1

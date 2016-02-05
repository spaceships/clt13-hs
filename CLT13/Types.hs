module CLT13.Types where

import qualified Data.Set as S

type IndexSet = S.Set Int

data Encoding = Encoding { enc_index :: IndexSet
                         , enc_value :: Integer
                         }

data Params = Params { lambda :: Int
                     , kappa  :: Int
                     , nzs    :: Int
                     , alpha  :: Int
                     , beta   :: Int
                     , eta    :: Int
                     , n      :: Int
                     , nu     :: Int
                     , rho    :: Int
                     } deriving (Show)

data MMap = MMap { params     :: Params
                 , ps         :: [Integer]
                 , gs         :: [Integer]
                 , zinvs      :: [Integer]
                 , crt_coeffs :: [Integer]
                 , pzt        :: Integer
                 } deriving (Show)


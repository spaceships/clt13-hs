{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}

module CLT13.MMap where

import CLT13.IndexSet
import CLT13.Rand
import CLT13.Util

import Control.Monad
import Data.Maybe (fromMaybe)
import Debug.Trace
import qualified Data.Map.Strict as M

data Params = Params {
    lambda :: Int,
    kappa  :: Int,
    nzs    :: Int,
    alpha  :: Int,
    beta   :: Int,
    eta    :: Int,
    n      :: Int,
    nu     :: Int,
    rho    :: Int
} deriving (Show)

data MMap = MMap {
    params     :: Params,
    ps         :: [Integer],
    gs         :: [Integer],
    zinvs      :: [Integer],
    crt_coeffs :: [Integer],
    pzt        :: Integer,
    x0         :: Integer,
    topLevel   :: IndexSet
} deriving (Show)

data PublicParams = PublicParams {
    modulus    :: Integer,
    zeroTester :: Integer,
    threshold  :: Int
}

publicParams :: MMap -> PublicParams
publicParams mmap = PublicParams (x0 mmap) (pzt mmap) (nu (params mmap))

genParams :: Int -> Int -> Int -> Maybe Int -> Params
genParams λ κ nzs n_ = Params λ κ nzs α β η n nu ρ
  where
    α   = λ
    β   = λ
    ρ   = λ
    ρ_f = κ * (ρ + α + 2)
    η   = ρ_f + α + 2*β + λ + 8
    nu  = η - β - ρ_f - λ - 3
    n   = fromMaybe (η * floor (logBase 2 (fromIntegral λ))) n_

setup :: Bool -> Int -> Int -> Int -> Maybe Int -> IndexSet -> IO MMap
setup verbose lambda_ kappa_ nzs_ n_ topLevelIndex = do
    when verbose $ putStrLn "generate the mmap parameters"
    let params = genParams lambda_ kappa_ nzs_ n_
        Params {..} = params
    when verbose $ print params

    when verbose $ putStrLn "generate the p_i's"
    ps <- randIO (genPs n eta)
    forceM ps

    when verbose $ putStrLn "multiply them to x0"
    let x0 = product ps
    forceM x0

    when verbose $ putStrLn "generate the g_i's"
    gs <- randIO (randPrimes n alpha)
    forceM gs

    when verbose $ putStrLn "generate the crt coeffs"
    let crt_coeffs = genCrtCoeffs ps x0
    forceM crt_coeffs

    when verbose $ putStrLn "generate the z_i's and zinvs"
    (zs, zinvs) <- unzip <$> randIO (randInvs nzs x0)
    forceM (zs, zinvs)

    when verbose $ putStrLn "generate zero-tester pzt"
    pzt <- randIO (genZeroTester n beta zs topLevelIndex gs ps x0)
    forceM pzt

    return $ MMap params ps gs zinvs crt_coeffs pzt x0 topLevelIndex

genPs :: Int -> Int -> Rand [Integer]
genPs n eta =
    if eta > 460 then do
        let eta'    = 460
            nchunks = eta `div` eta'
        traceShowM nchunks
        forM [0..n-1] $ \i -> do
            let size = if i < n-1 then eta' else eta - (n-1)*eta'
            chunks <- randPrimes nchunks size
            return (product chunks)
    else do
        randPrimes n eta

genCrtCoeffs :: [Integer] -> Integer -> [Integer]
genCrtCoeffs ps x0 = pmap crt_coeff ps
  where
    crt_coeff p =
        let q = x0 `div` p
        in q * invMod q p `mod` x0

genZeroTester
  :: Int
  -> Int
  -> [Integer]
  -> IndexSet
  -> [Integer]
  -> [Integer]
  -> Integer
  -> Rand Integer
genZeroTester n beta zs pows gs ps x0 = do
    hs <- replicateM n (randInteger beta)
    let xs = pmap forloop (zip3 gs ps hs)
    return (sumMod xs x0)
  where
    zkappa = prodMod (getPows zs pows) x0

    getPows :: [Integer] -> IndexSet -> [Integer]
    getPows zs pows = getPows' (zip [0..] zs) pows

    getPows' :: [(Int, Integer)] -> IndexSet -> [Integer]
    getPows' [] _ = []
    getPows' ((i,z):zs) pows = case M.lookup i pows of
        Nothing  -> getPows' zs pows
        Just pow -> (z^pow `mod` x0) : getPows' zs pows

    forloop (g, p, h) =
        (invMod g p * zkappa `mod` p) * h * (x0 `div` p)

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

module CLT13.MMap where

import CLT13.IndexSet
import CLT13.Rand
import CLT13.Util

import Control.Monad
import Data.Maybe (fromMaybe)
import Debug.Trace
import Text.Printf
import qualified Data.Map.Strict as M
import qualified GHC.Integer.GMP.Internals as GMP

#if OPTIMIZATION_CRT_TREE
data CrtTree = CrtNode {
    crt_n     :: Int,
    left      :: CrtTree,
    right     :: CrtTree,
    crt_left  :: Integer,
    crt_right :: Integer,
    crt_mod   :: Integer
} | CrtLeaf Integer
  deriving (Show)

crtMod :: CrtTree -> Integer
crtMod (CrtNode { crt_mod }) = crt_mod
crtMod (CrtLeaf x) = x
#endif

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
    gs         :: [Integer],
    zinvs      :: [Integer],
    pzt        :: Integer,
    x0         :: Integer,
    topLevel   :: IndexSet,
#if OPTIMIZATION_CRT_TREE
    crt :: CrtTree
#else
    crt_coeffs :: [Integer]
#endif
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
setup verbose lambda_ kappa_ nzs_ n_ topLevel = do
    when verbose $ putStrLn "generate the mmap parameters"
    let params = genParams lambda_ kappa_ nzs_ n_
        Params {..} = params
    when verbose $ print params

#if OPTIMIZATION_CRT_TREE
    let loop = do when verbose $ putStrLn "generate the p_i's"
                  ps <- randIO (genPs verbose n kappa eta)
                  forceM ps
                  when verbose $ putStrLn "generate the crt tree"
                  case genCrtTree ps of
                      Nothing  -> do
                          when verbose $ putStrLn "crt tree failed: restarting"
                          loop
                      Just crt -> do
                          let x0 = crtMod crt
                          return (ps, x0, crt)

    (ps, x0, crt) <- loop
#else
    when verbose $ putStrLn "generate the p_i's"
    ps <- randIO (genPs verbose n kappa eta)
    forceM ps

    when verbose $ putStrLn "multiply them to x0"
    let x0 = product ps
    forceM x0

    when verbose $ putStrLn "generate the crt coeffs"
    let crt_coeffs = genCrtCoeffs ps x0
    forceM crt_coeffs
#endif

    when verbose $ putStrLn "generate the g_i's"
    gs <- randIO (randPrimes n alpha)
    forceM gs

    when verbose $ putStrLn "generate the z_i's and zinvs"
    (zs, zinvs) <- unzip <$> randIO (randInvs nzs x0)
    forceM (zs, zinvs)

    when verbose $ putStrLn "generate zero-tester pzt"
    pzt <- randIO (genZeroTester n beta zs topLevel gs ps x0)
    forceM pzt

    return $ MMap { params
                  , gs
                  , zinvs
                  , pzt
                  , x0
                  , topLevel
#if OPTIMIZATION_CRT_TREE
                  , crt
#else
                  , crt_coeffs
#endif
                  }

genPs :: Bool -> Int -> Int -> Int -> Rand [Integer]
genPs verbose n kappa eta =
#if OPTIMIZATION_COMPOSITE_PS
    if eta > 10*kappa then do
        let eta' = head $ filter ((>2*kappa ) . mod eta) (iterate (+100) 420)
        when verbose $ traceM ("eta' = " ++ show eta')
        when verbose $ traceM ("eta % eta' = " ++ show (mod eta eta'))
        let nchunks = ceiling (fromIntegral eta / fromIntegral eta')
        when verbose $ traceM ("nchunks = " ++ show nchunks)
        forM [0..n-1] $ \_ -> do
            chunks    <- randPrimes (nchunks-1) eta'
            lastChunk <- randPrimes 1 (eta - (nchunks-1)*eta')
            forceM (chunks ++ lastChunk)
            return (product (chunks ++ lastChunk))
    else do
#endif
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

#if OPTIMIZATION_CRT_TREE
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt 0 b = (b, 0, 1)
gcdExt a b = let (g, s, t) = gcdExt (b `mod` a) a
             in (g, t - (b `div` a) * s, s)

genCrtTree :: [Integer] -> Maybe CrtTree
genCrtTree [x] = Just (CrtLeaf x)
genCrtTree ps  = do
    let n = length ps `div` 2
    left  <- genCrtTree (take n ps)
    right <- genCrtTree (drop n ps)
    let (g, crt_right, crt_left) = gcdExt (crtMod left) (crtMod right)
    guard (g == 1)
    let crt_mod' = crtMod left * crtMod right
    return $ CrtNode { crt_n = length ps, left, right, crt_left, crt_right, crt_mod = crt_mod' }

doCrt :: CrtTree -> [Integer] -> Integer
doCrt (CrtLeaf _)    [x] = x
doCrt (CrtNode {..}) xs  =
    if (length xs /= crt_n) then
        error "[doCrt] weird length xs"
    else
        (val_left * crt_left + val_right * crt_right) `mod` crt_mod
  where
    n = length xs `div` 2
    val_left  = doCrt left  (take n xs)
    val_right = doCrt right (drop n xs)
#endif

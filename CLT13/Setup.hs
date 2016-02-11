{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module CLT13.Setup where

import CLT13.Types
import CLT13.Util
import CLT13.Rand

import Control.Monad
import qualified Data.Map as M

genParams :: Int -> Int -> Int -> Params
genParams lambda kappa nzs = Params lambda kappa nzs alpha beta eta n nu rho
    where
        alpha = lambda
        beta  = lambda
        rho   = lambda
        rho_f = kappa * (rho + alpha + 2)
        eta   = rho_f + alpha + 2*beta + lambda + 8
        nu    = eta - beta - rho_f - lambda - 3
        n     = eta * floor (logBase 2 (fromIntegral lambda))

setup :: Int -> Int -> Int -> IndexSet -> IO MMap
setup lambda_ kappa_ nzs_ topLevelIndex = do
    putStrLn("generate the mmap parameters")
    let params = genParams lambda_ kappa_ nzs_
        Params {..} = params
    print params

    putStrLn("generate the p_i's")
    ps <- randIO (randPrimes n eta)
    forceM ps

    putStrLn("multiply them to x0")
    let x0 = product ps
    forceM x0

    putStrLn("generate the g_i's")
    gs <- randIO (randPrimes n alpha)
    forceM gs

    putStrLn("generate the crt coeffs")
    let crt_coeffs = genCrtCoeffs ps x0
    forceM crt_coeffs

    putStrLn("generate the z_i's and zinvs")
    (zs, zinvs) <- unzip <$> randIO (randInvs nzs x0)
    forceM (zs, zinvs)

    putStrLn("generate zero-tester pzt")
    pzt <- randIO (genZeroTester n beta zs topLevelIndex gs ps x0)
    forceM pzt

    return $ MMap params ps gs zinvs crt_coeffs pzt x0

genCrtCoeffs :: [Integer] -> Integer -> [Integer]
genCrtCoeffs ps x0 = pmap crt_coeff ps
    where
        crt_coeff p = let q = x0 `div` p
                      in q * invMod q p `mod` x0

genZeroTester :: Int -> Int -> [Integer] -> IndexSet -> [Integer] -> [Integer] -> Integer -> Rand Integer
genZeroTester n beta zs pows gs ps x0 = do
        hs <- replicateM n (randInteger beta)
        let xs = pmap forloop (zip3 gs ps hs)
        return (sumMod xs x0)
    where
        zkappa = prodMod (getPows zs pows) x0
        forloop (g, p, h) = (invMod g p * zkappa `mod` p) * h * (x0 `div` p) `mod` x0

        getPows :: [Integer] -> IndexSet -> [Integer]
        getPows zs pows = getPows' (zip [0..] zs) pows

        getPows' :: [(Int, Integer)] -> IndexSet -> [Integer]
        getPows' [] _ = []
        getPows' ((i,z):zs) pows = case M.lookup i pows of
            Nothing  -> getPows' zs pows
            Just pow -> z ^ pow : getPows' zs pows

{-# LANGUAGE RecordWildCards #-}

module CLT13 where

import CLT13.Util

import Control.Monad
import qualified Data.Set as S
import qualified GHC.Integer.GMP.Internals as GMP
import Control.Parallel.Strategies

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
                 }

genParams :: Int -> Int -> Int -> Params
genParams lambda kappa nzs = Params lambda kappa nzs alpha beta eta n nu rho
    where
        alpha = lambda
        beta  = lambda
        rho   = lambda
        rho_f = kappa * (rho + alpha + 2)
        eta   = rho_f + alpha + 2 * beta + lambda + 8
        nu    = eta - beta - rho_f - lambda - 3
        n     = eta * floor (logBase 2 (fromIntegral lambda))

setup :: Int -> Int -> Int -> IO MMap
setup lambda_ kappa_ nzs_ = do
    putStrLn("generate the mmap parameters")
    let params = genParams lambda_ kappa_ nzs_
        Params {..} = params
    print params

    putStrLn("generate the p_i's and x0")
    ps <- randIO (randPrimes n eta)
    let x0 = sum ps

    putStrLn("generate the crt coeffs")
    let crt_coeffs = genCrtCoeffs ps x0

    putStrLn("generate the g_i's")
    gs <- randIO (randPrimes n alpha)

    putStrLn("generate the z_i's and zinvs")
    (zs, zinvs) <- unzip <$> randInvsIO nzs eta x0

    return $ MMap params ps gs zinvs crt_coeffs undefined

    {-putStrLn("generate zero-tester pzt")-}
    {-pzt <- generateZeroTester beta zs gs ps x0-}
    {-return $ MMap params ps gs zinvs crt_coeffs pzt-}

genCrtCoeffs :: [Integer] -> Integer -> [Integer]
genCrtCoeffs ps x0 = map crt_coeff ps `using` rdeepseq
    where
        crt_coeff p = let q = x0 `div` p
                      in q * invMod q p

{-generateZeroTester :: Int -> [Integer] -> [Integer] -> [Integer] -> Integer -> IO Integer-}
{-generateZeroTester beta zs gs ps x0 = do-}
        {-xs <- forM (zip gs ps) $ \(g, p) -> do-}
            {-let ginv = invMod g p-}
            {-h <- randInteger beta-}
            {-return $ mulMod ginv zkappa p * h * (x0 `div` p)-}
        {-return $ foldr (\x y -> addMod x y x0) 0 xs-}
    {-where-}
        {-zkappa = foldr (\x y -> mulMod x y x0) 1 zs-}

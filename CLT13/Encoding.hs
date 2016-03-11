{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}

module CLT13.Encoding where

import CLT13.IndexSet
import CLT13.MMap
import CLT13.Rand
import CLT13.Util

import Control.Monad
import Control.Parallel.Strategies (NFData)
import Data.List (zip4)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Data.Map as M

import Debug.Trace

data Encoding = Encoding {
    ix  :: IndexSet,
    val :: Integer
} deriving (Generic, NFData, Serialize)

encode :: [Integer] -> IndexSet -> MMap -> Rand Encoding
encode ms ix mmap
    | length ms < n = encode (ms ++ replicate (n - length ms) 0) ix mmap
    | otherwise     = do
        rs <- replicateM n (randInteger rho)
        let zinv = prodMod [ (zinvs!!i)^pow | (i,pow) <- M.toList ix ] x0
#if OPTIMIZATION_CRT_TREE
        let c = doCrt crt [ (mod m g + r * g) | m <- ms | g <- gs | r <- rs ]
#else
        let c = sumMod [ (mod m g + r * g) * coeff | m <- ms | g <- gs | r <- rs | coeff <- crt_coeffs ] x0
#endif
        return $ Encoding ix (mulMod c zinv x0)
    where
        MMap   {..} = mmap
        Params {..} = params

isZero :: PublicParams -> Encoding -> Bool
isZero (PublicParams {..}) c = sizeBase2 x < sizeBase2 modulus - threshold
    where
        x = modNear (val c * zeroTester) modulus
        modNear x q = let x' = x `mod` modulus
                      in if x' > div modulus 2 then x' - modulus else x'

add :: PublicParams -> Encoding -> Encoding -> Encoding
add pp x y | indexNeq (ix x) (ix y) = error "[add] cannot add encodings with different indices!"
           | otherwise = Encoding (ix x) val'
  where
    val' = val x + val y `mod` modulus pp

sub :: PublicParams -> Encoding -> Encoding -> Encoding
sub pp x y | indexNeq (ix x) (ix y) = error "[sub] cannot subtract encodings with different indices!"
           | otherwise = Encoding (ix x) val'
  where
    val' = val x - val y `mod` modulus pp


mul :: PublicParams -> Encoding -> Encoding -> Encoding
mul pp x y = Encoding ix' val'
  where
    ix'  = indexUnion (ix x) (ix y)
    val' = val x * val y `mod` modulus pp

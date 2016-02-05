{-# LANGUAGE RecordWildCards #-}

module CLT13.Encoding where

import CLT13.Types
import CLT13.Util

import Control.Monad
import Data.List (zip4)
import qualified Data.Set as S

type Encoding = Integer

encode :: [Integer] -> IndexSet -> MMap -> Rand Encoding
encode ms ix mmap
    | length ms < n = encode (ms ++ replicate (n - length ms) 0) ix mmap
    | otherwise     = do
        rs <- replicateM n (randInteger rho)
        let cs = map (\(m, g, r, crt) -> (m + r*g) * crt) (zip4 ms gs rs crt_coeffs)
            c  = sum cs
            zs = map (zinvs !!) (S.toList ix)
        return (foldl (\x y -> x * y `mod` x0) c zs)
    where
        MMap   {..} = mmap
        Params {..} = params

isZero :: Encoding -> Integer -> Integer -> Int -> Bool
isZero c pzt x0 nu = sizeBase2 x < sizeBase2 x0 - nu
    where
        x = modNear (c*pzt) x0
        modNear x q = let x' = x `mod` q
                      in if x' > div q 2 then x'-q else x'

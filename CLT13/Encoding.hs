{-# LANGUAGE RecordWildCards #-}

module CLT13.Encoding where

import CLT13.IndexSet
import CLT13.MMap
import CLT13.Rand
import CLT13.Util

import Control.Monad
import Data.List (zip4)
import qualified Data.Map as M

type Encoding = Integer

encode :: [Integer] -> IndexSet -> MMap -> Rand Encoding
encode ms ix mmap
    | length ms < n = encode (ms ++ replicate (n - length ms) 0) ix mmap
    | otherwise     = do
        rs <- replicateM n (randInteger rho)
        let cs = map (\(m, g, r, crt) -> (mod m g + r * g) * crt) (zip4 ms gs rs crt_coeffs)
            c  = sum cs
            zs = map (\(i,pow) -> (zinvs!!i)^pow) (M.toList ix)
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

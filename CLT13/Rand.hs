{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}

module CLT13.Rand where

import CLT13.Util

import Crypto.Random
import Crypto.Random.DRBG
import Crypto.Util (bs2i)

import Control.Arrow (first)
import Control.Monad.State.Strict
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified GHC.Integer.GMP.Internals as GMP

type Rng  = CtrDRBG
type Rand = State Rng

randInteger_ :: Rng -> Int -> (Integer, Rng)
randInteger_ gen nbits = case genBytes nbytes gen of
    Left err -> error ("[randInteger_] " ++ show err)
    Right t  -> first (bs2i . truncate) t

  where
    overflow = nbits `mod` 8
    nbytes   = ceiling (fromIntegral nbits / 8)

    truncate :: BS.ByteString -> BS.ByteString
    truncate bs = BS.cons w'' (BS.tail bs)
        where
            w   = BS.head bs
            w'  = w .&. (2^(nbits `mod` 8) - 1)
            w'' = if overflow == 0 then w else w'

runRand :: Rand a -> Rng -> (a, Rng)
runRand = runState

randIO :: Rand a -> IO a
randIO m = do
    gen <- newGenIO
    let (x,_) = runRand m gen
    return x

randInteger :: Int -> Rand Integer
randInteger nbits = do
    rng <- get
    let (x, rng') = randInteger_ rng nbits
    put rng'
    return x

randIntegerMod :: Integer -> Rand Integer
randIntegerMod q = do
    let nbits = sizeBase2 q
    x <- randInteger nbits
    if x >= q then
        randIntegerMod q
    else
        return x

randPrimes :: Int -> Int -> Rand [Integer]
randPrimes nprimes nbits = do
    rngs <- splitRand nprimes
    let rs = pmap (fst . flip randInteger_ nbits) rngs
        ps = pmap GMP.nextPrimeInteger rs
    return ps

randInv :: Integer -> Rand (Integer, Integer)
randInv q = try 100
  where
    try 0 = error "[randInv] ran out of tries!"
    try n = do
        x <- randIntegerMod q
        let xinv = invMod x q
        if xinv == 0
            then try (n-1)
            else return (x, xinv)

randInvs :: Int -> Integer -> Rand [(Integer, Integer)]
randInvs ninvs modulus = do
    rngs <- splitRand ninvs
    let invs = pmap fst (map (runRand (randInv modulus)) rngs)
    return invs

splitRand :: Int -> Rand [Rng]
splitRand 0 = return []
splitRand n = do
    gen <- get
    case splitGen gen of
        Left err      -> error ("[splitGen] " ++ show err)
        Right (g0,g1) -> do
            put g0
            rest <- splitRand (n-1)
            return (g1:rest)

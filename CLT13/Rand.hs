{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}

module CLT13.Rand where

import CLT13.Util

import Control.Monad.State.Strict
import Crypto.Random
import Crypto.Util (bs2i)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified GHC.Integer.GMP.Internals as GMP

type Rng  = SystemRandom
type Rand = State Rng

randInteger_ :: Rng -> Int -> (Integer, Rng)
randInteger_ gen nbits = either (error . show) (\(bs, g) -> (bs2i (truncate bs), g)) (genBytes nbytes gen)
    where
        overflow = nbits `mod` 8
        nbytes   = ceiling (fromIntegral nbits / 8)

        truncate :: BS.ByteString -> BS.ByteString
        truncate bs = BS.cons w'' (BS.tail bs)
            where
                w   = BS.head bs
                w'  = w .&. (2 ^ (nbits `mod` 8) - 1)
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
    !rs <- replicateM nprimes (randInteger nbits)
    let ps = pmap GMP.nextPrimeInteger rs
    return ps

randInv :: Integer -> Rand (Integer, Integer)
randInv q = do
    x <- randIntegerMod q
    let xinv = invMod x q
        one  = x * xinv `mod` q
    if one /= 1
        then randInv q
        else return (x, xinv)

randInvsIO :: Int -> Integer -> IO [(Integer, Integer)]
randInvsIO ninvs modulus = do
    rngs <- replicateM ninvs newGenIO
    let invs = pmap fst (map (runRand (randInv modulus)) rngs)
    return invs


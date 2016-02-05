{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE BangPatterns #-}

module CLT13.Util where

import System.IO

import Control.DeepSeq (deepseq)
import Data.Word
import qualified Data.Bits as B
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified GHC.Integer.GMP.Internals as GMP
import Control.Parallel.Strategies
import Control.Monad
import Crypto.Random
import Crypto.Util (bs2i)
import Control.Monad.State.Strict

type Rng = SystemRandom
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

randPrimes :: Int -> Int -> Rand [Integer]
randPrimes nprimes nbits = do
    !rs <- replicateM nprimes (randInteger nbits)
    let ps = pmap GMP.nextPrimeInteger rs
    return ps

randInv :: Int -> Integer -> Rand (Integer, Integer)
randInv nbits modulus = do
    r <- randInteger nbits
    let rinv = invMod r modulus
    if r >= modulus || rinv == 0
        then randInv nbits modulus
        else return (r, rinv)

randInvsIO :: Int -> Int -> Integer -> IO [(Integer, Integer)]
randInvsIO ninvs nbits modulus = do
    rngs <- replicateM ninvs newGenIO
    let invs = pmap fst (map (runRand (randInv nbits modulus)) rngs)
    return invs

sizeBase2 :: Integer -> Int
sizeBase2 x = ceiling (logBase 2 (fromIntegral x))

invMod :: Integer -> Integer -> Integer
invMod x q = GMP.recipModInteger x q

mulMod :: Integer -> Integer -> Integer -> Integer
mulMod x y z = x * y `mod` z

addMod :: Integer -> Integer -> Integer -> Integer
addMod x y z = x + y `mod` z

sumMod :: [Integer] -> Integer -> Integer
sumMod xs q = foldl (\x y -> addMod x y q) 0 xs

prodMod :: [Integer] -> Integer -> Integer
prodMod xs q = foldl (\x y -> mulMod x y q) 1 xs

pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap = parMap rdeepseq

forceM :: (Monad m, NFData a) => a -> m ()
forceM x = x `deepseq` return ()

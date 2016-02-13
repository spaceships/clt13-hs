{-# LANGUAGE MagicHash #-}

module CLT13.Util where

import Control.DeepSeq (deepseq)
import Control.Parallel.Strategies
import GHC.Prim
import GHC.Types
import qualified GHC.Integer.GMP.Internals as GMP

sizeBase2 :: Integer -> Int
sizeBase2 x = fromIntegral (W# (GMP.sizeInBaseInteger x 2#))

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

plist :: NFData n => [n] -> [n]
plist = withStrategy (parList rdeepseq)

forceM :: (Monad m, NFData a) => a -> m ()
forceM x = x `deepseq` return ()

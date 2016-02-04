module CLT13.Util where

import System.IO

import Data.Word
import qualified Data.Bits as B
import Data.Bits ((.&.))
import System.Entropy
import qualified Data.ByteString as BS
import qualified GHC.Integer.GMP.Internals as GMP

randInteger :: Int -> IO Integer
randInteger nbits = do
    let nbytes   = ceiling (fromIntegral nbits / 8)
        overflow = nbits `mod` 8
    (w:ws) <- BS.unpack <$> getEntropy nbytes
    let w' = if overflow == 0 then w else w .&. (2 ^ (nbits `mod` 8) - 1)
    return $ words2Integer (w':ws)

words2Integer :: [Word8] -> Integer
words2Integer ws = foldr (\(x, pow) z -> z + B.shift (fromIntegral x) pow) 0 (zip (reverse ws) [0,8..])

randPrime :: Int -> IO Integer
randPrime nbits = do
    r <- randInteger nbits
    return (GMP.nextPrimeInteger r)

randInvertible :: Int -> Integer -> IO (Integer, Integer)
randInvertible nbits modulus = loop
    where
        loop = do
            r <- randInteger nbits
            let rinv = invMod r modulus
            if r >= modulus || rinv == 0
                then putStrLn "loop" >> loop
                else return (r, rinv)

sizeBase2 :: Integer -> Int
sizeBase2 x = ceiling (logBase 2 (fromIntegral x))

invMod :: Integer -> Integer -> Integer
invMod x q = GMP.recipModInteger x q

mulMod :: Integer -> Integer -> Integer -> Integer
mulMod x y z = x * y `mod` z

addMod :: Integer -> Integer -> Integer -> Integer
addMod x y z = x + y `mod` z

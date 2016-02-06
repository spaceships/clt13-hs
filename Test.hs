{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import CLT13
import CLT13.Util
import CLT13.Types
import CLT13.Encoding

import Text.Printf
import System.CPUTime
import Control.Parallel.Strategies

main = do
    let lambda = 40
        kappa  = 2
        nzs    = 10
    mmap <- setup lambda kappa nzs
    let MMap   { x0, pzt, params } = mmap
    let Params { nu }              = params

    x <- randIO (randInteger (alpha params))
    putStrLn ("x = " ++ show x)

    c0 <- time $ randIO (encode [0] [0..nzs-1] mmap)
    c1 <- time $ randIO (encode [0] [0..nzs-1] mmap)
    let c2 = c0 + c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(0 + 0)" r True

    c0 <- randIO (encode [0] [0..nzs-1] mmap)
    c1 <- randIO (encode [1] [0..nzs-1] mmap)
    let c2 = c0 + c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(0 + 1)" r False

    c0 <- randIO (encode [0] [0..nzs-1] mmap)
    c1 <- randIO (encode [x] [0..nzs-1] mmap)
    let c2 = c0 + c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(0 + x)" r False

    c0 <- randIO (encode [x] [0..nzs-1] mmap)
    c1 <- randIO (encode [x] [0..nzs-1] mmap)
    let c2 = c0 - c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(x - x)" r True

    c0 <- randIO (encode [0] [0..nzs-1] mmap)
    c1 <- randIO (encode [x] [0..nzs-1] mmap)
    let c2 = c0 - c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(0 - x)" r False

    c0 <- randIO (encode [1] [0..nzs-1] mmap)
    c1 <- randIO (encode [0] [0..nzs-1] mmap)
    let c2 = c0 - c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(1 - 0)" r False

    c0 <- randIO (encode [x] [0..div nzs 2 - 1] mmap)
    c1 <- randIO (encode [0] [div nzs 2..nzs-1] mmap)
    let c2 = c0 * c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(x * 0)" r True

    c0 <- randIO (encode [x] [0..div nzs 2 - 1] mmap)
    c1 <- randIO (encode [1] [div nzs 2..nzs-1] mmap)
    let c2 = c0 * c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(x * 1)" r False

    c0 <- randIO (encode [x] [0..div nzs 2 - 1] mmap)
    c1 <- randIO (encode [x] [div nzs 2..nzs-1] mmap)
    let c2 = c0 * c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(x * x)" r False

    return ()

time :: NFData a => IO a -> IO a
time a = do
    start <- getCPUTime
    v <- a
    forceM v
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

expect :: (Show a, Eq a) => String -> a -> a -> IO ()
expect string got expected
    | got == expected = putStrLn (string ++ " = " ++ show got)
    | otherwise       = putStrLn ("\x1b[1;41m" ++ string ++ " = " ++ show got ++ "\x1b[0m")

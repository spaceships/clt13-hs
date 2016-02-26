{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import CLT13
import CLT13.Rand
import CLT13.Util

import Text.Printf
import System.CPUTime
import Control.Parallel.Strategies
import qualified Data.Map as M

main = do
    let lambda   = 20
        kappa    = 2
        nzs      = 10
        topLevel = M.fromList [ (i,1) | i <- [0..nzs-1] ]
    {-let lambda   = 17-}
        {-kappa    = 200-}
        {-nzs      = 50-}
        {-n        = Just 2-}
        {-topLevel = M.fromList [ (i,1) | i <- [0..nzs-1] ]-}

    mmap <- setup True lambda kappa nzs n topLevel
    let pp = publicParams mmap
    let MMap { x0, params } = mmap

    x <- randIO (randInteger lambda)
    putStrLn ("x = " ++ show x)

    c0 <- randIO (encode [0] topLevel mmap)
    c1 <- randIO (encode [0] topLevel mmap)
    let c2 = add pp c0 c1
        r  = isZero pp c2
    expect "isZero(0 + 0)" r True

    c0 <- randIO (encode [0] topLevel mmap)
    c1 <- randIO (encode [1] topLevel mmap)
    let c2 = add pp c0 c1
        r  = isZero pp c2
    expect "isZero(0 + 1)" r False

    c0 <- randIO (encode [0] topLevel mmap)
    c1 <- randIO (encode [x] topLevel mmap)
    let c2 = add pp c0 c1
        r  = isZero pp c2
    expect "isZero(0 + x)" r False

    c0 <- randIO (encode [x] topLevel mmap)
    c1 <- randIO (encode [x] topLevel mmap)
    let c2 = sub pp c0 c1
        r  = isZero pp c2
    expect "isZero(x - x)" r True

    c0 <- randIO (encode [0] topLevel mmap)
    c1 <- randIO (encode [x] topLevel mmap)
    let c2 = sub pp c0 c1
        r  = isZero pp c2
    expect "isZero(0 - x)" r False

    c0 <- randIO (encode [1] topLevel mmap)
    c1 <- randIO (encode [0] topLevel mmap)
    let c2 = sub pp c0 c1
        r  = isZero pp c2
    expect "isZero(1 - 0)" r False

    let firstHalf  = M.fromList [ (i,1) | i <- [0..div nzs 2 - 1] ] :: IndexSet
        secondHalf = M.fromList [ (i,1) | i <- [div nzs 2..nzs-1] ] :: IndexSet

    c0 <- randIO (encode [x] firstHalf  mmap)
    c1 <- randIO (encode [0] secondHalf mmap)
    let c2 = mul pp c0 c1
        r  = isZero pp c2
    expect "isZero(x * 0)" r True

    c0 <- randIO (encode [x] firstHalf  mmap)
    c1 <- randIO (encode [1] secondHalf mmap)
    let c2 = mul pp c0 c1
        r  = isZero pp c2
    expect "isZero(x * 1)" r False

    c0 <- randIO (encode [x] firstHalf  mmap)
    c1 <- randIO (encode [x] secondHalf mmap)
    let c2 = mul pp c0 c1
        r  = isZero pp c2
    expect "isZero(x * x)" r False

    -- zimmerman-like tests
    y0 <- randIO (randInteger lambda)
    y1 <- randIO (randInteger lambda)
    let y2 = y0 * y1
    c0 <- randIO (encode [x,y0] firstHalf  mmap)
    c1 <- randIO (encode [0,y1] secondHalf mmap)
    c  <- randIO (encode [0,y2] topLevel   mmap)
    let c2 = mul pp c0 c1
        c3 = sub pp c2 c
        r  = isZero pp c3
    expect "[Z] isZero(x * 0)" r True

    y0 <- randIO (randInteger lambda)
    y1 <- randIO (randInteger lambda)
    x' <- randIO (randInteger lambda)
    let y2 = y0 * y1
    c0 <- randIO (encode [x ,y0] firstHalf  mmap)
    c1 <- randIO (encode [x',y1] secondHalf mmap)
    c  <- randIO (encode [0 ,y2] topLevel   mmap)
    let c2 = mul pp c0 c1
        c3 = sub pp c2 c
        r  = isZero pp c3
    expect "[Z] isZero(x * y)" r False

    α  <- randIO (randInteger lambda)
    β  <- randIO (randInteger lambda)
    c0 <- randIO (encode [α ,β] topLevel  mmap)
    c1 <- randIO (encode [α ,β] topLevel  mmap)
    let c2 = sub pp c0 c1
        r  = isZero pp c2
    expect "[Z] isZero(x - x)" r True

    α  <- randIO (randInteger lambda)
    β  <- randIO (randInteger lambda)
    c0 <- randIO (encode [α ,β] topLevel  mmap)
    c1 <- randIO (encode [0 ,β] topLevel  mmap)
    c2 <- randIO (encode [α ,0] topLevel  mmap)
    let c3 = sub pp c0 c1
        c4 = sub pp c3 c2
        r  = isZero pp c4
    expect "[Z] isZero([a,b] - [0,b] - [a,0])" r True

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

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
    let lambda   = 40
        kappa    = 2
        nzs      = 10
        topLevel = M.fromList [ (i,1) | i <- [0..nzs-1] ]
    {-let lambda   = 4-}
        {-kappa    = 29-}
        {-nzs      = 153-}
        {-topLevel = M.fromList [ (i,1) | i <- [0..nzs-1] ]-}

    mmap <- setup True lambda kappa nzs topLevel
    let MMap   { x0, pzt, params } = mmap
    let Params { nu }              = params

    x <- randIO (randInteger lambda)
    putStrLn ("x = " ++ show x)

    c0 <- time $ randIO (encode [0] topLevel mmap)
    c1 <- time $ randIO (encode [0] topLevel mmap)
    let c2 = c0 + c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(0 + 0)" r True

    c0 <- randIO (encode [0] topLevel mmap)
    c1 <- randIO (encode [1] topLevel mmap)
    let c2 = c0 + c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(0 + 1)" r False

    c0 <- randIO (encode [0] topLevel mmap)
    c1 <- randIO (encode [x] topLevel mmap)
    let c2 = c0 + c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(0 + x)" r False

    c0 <- randIO (encode [x] topLevel mmap)
    c1 <- randIO (encode [x] topLevel mmap)
    let c2 = c0 - c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(x - x)" r True

    c0 <- randIO (encode [0] topLevel mmap)
    c1 <- randIO (encode [x] topLevel mmap)
    let c2 = c0 - c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(0 - x)" r False

    c0 <- randIO (encode [1] topLevel mmap)
    c1 <- randIO (encode [0] topLevel mmap)
    let c2 = c0 - c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(1 - 0)" r False

    let firstHalf  = M.fromList [ (i,1) | i <- [0..div nzs 2 - 1] ] :: IndexSet
        secondHalf = M.fromList [ (i,1) | i <- [div nzs 2..nzs-1] ] :: IndexSet

    c0 <- randIO (encode [x] firstHalf  mmap)
    c1 <- randIO (encode [0] secondHalf mmap)
    let c2 = c0 * c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(x * 0)" r True

    c0 <- randIO (encode [x] firstHalf  mmap)
    c1 <- randIO (encode [1] secondHalf mmap)
    let c2 = c0 * c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(x * 1)" r False

    c0 <- randIO (encode [x] firstHalf  mmap)
    c1 <- randIO (encode [x] secondHalf mmap)
    let c2 = c0 * c1 `mod` x0
        r  = isZero c2 pzt x0 nu
    expect "isZero(x * x)" r False

    -- zimmerman-like tests
    y0 <- randIO (randInteger lambda)
    y1 <- randIO (randInteger lambda)
    let y2 = y0 * y1
    c0 <- randIO (encode [x,y0] firstHalf  mmap)
    c1 <- randIO (encode [0,y1] secondHalf mmap)
    c  <- randIO (encode [0,y2] topLevel   mmap)
    let c2 = c0 * c1 `mod` x0
        c3 = c2 - c  `mod` x0
        r  = isZero c3 pzt x0 nu
    expect "[Z] isZero(x * 0)" r True

    y0 <- randIO (randInteger lambda)
    y1 <- randIO (randInteger lambda)
    x' <- randIO (randInteger lambda)
    let y2 = y0 * y1
    c0 <- randIO (encode [x ,y0] firstHalf  mmap)
    c1 <- randIO (encode [x',y1] secondHalf mmap)
    c  <- randIO (encode [0 ,y2] topLevel   mmap)
    let c2 = c0 * c1 `mod` x0
        c3 = c2 - c  `mod` x0
        r  = isZero c3 pzt x0 nu
    expect "[Z] isZero(x * y)" r False

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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import CLT13
import CLT13.Util
import CLT13.Types
import CLT13.Encoding

main = do
    let lambda = 30
        kappa  = 1
        nzs    = 10
    mmap <- setup lambda kappa nzs

    c0 <- randIO (encode [0] [0..nzs-1] mmap)

    print (isZero c0 (pzt mmap) (x0 mmap) (nu (params mmap)))

    return ()

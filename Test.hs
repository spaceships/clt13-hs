{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NamedFieldPuns #-}

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
    let MMap   { x0, pzt, params } = mmap
    let Params { nu }              = params

    c0 <- randIO (encode [0,1,0,1] [0..nzs-1] mmap)


    print (isZero c0 pzt x0 nu)

    return ()

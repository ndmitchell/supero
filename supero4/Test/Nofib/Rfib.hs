-- !!! the ultra-notorious "nfib 30" does w/ Floats
--
module Test.Nofib.Rfib(test) where

nfib :: Double -> Double
nfib n = if n <= 1 then 1 else nfib (n-1) + nfib (n-2) + 1

root x = nfib x

#if MAIN
test = (\i -> root (i :: Double) :: Double, 30 :: Double)
#endif

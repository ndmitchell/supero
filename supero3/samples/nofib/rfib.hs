-- !!! the ultra-notorious "nfib 30" does w/ Floats
--
module Main (main) where
import System


nfib :: Double -> Double
nfib n = if n <= 1 then 1 else nfib (n-1) + nfib (n-2) + 1

root x = nfib x

#if MAIN
main = print $ root 30

ltEqDouble'2 = (<=) :: Double -> Double -> Bool
addDouble'2 = (+) :: Double -> Double -> Double
subDouble'2 = (-) :: Double -> Double -> Double
#endif

#if SUPERO
(+) = addDouble'2
(-) = subDouble'2
(<=) = ltEqDouble'2
#endif

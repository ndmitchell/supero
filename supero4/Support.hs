
module Support(
    tests, test,
    benchmarks, benchmark
    ) where

import Util
import Control.Monad
import Criterion.Main


tests :: [IO ()] -> IO ()
tests = sequence_

test :: String -> IO () -> IO () -> IO ()
test name orig opt = do
    putStr $ "Testing " ++ name ++ "... "
    a <- captureOutput orig
    b <- captureOutput opt
    when (a /= b) $ do
        putStrLn "\nFATAL: Results do not match"
        putStrLn "WANTED:"
        putStrLn a
        putStrLn "GOT:"
        putStrLn b
    putStrLn "success"


benchmarks :: [Benchmark] -> IO ()
benchmarks = defaultMain

benchmark :: String -> IO () -> IO () -> Benchmark
benchmark name orig opt = bgroup name [bench "GHC" orig, bench "Supero" opt]


module Support(
    tests, test,
    benchmarks, benchmark
    ) where

import Util
import Control.Monad
import Criterion.Main
import Control.DeepSeq
import Control.Exception
import System.IO


tests :: [IO ()] -> IO ()
tests xs = do
    hSetBuffering stdout NoBuffering
    putStrLn $ "Testing " ++ show (length xs)
    sequence_ xs
    putStrLn "Test completed successfully"

test :: (NFData b, Show b, Eq b) => String -> (a -> b, a) -> (a -> b, a) -> IO ()
test name orig opt = do
    putStr $ "Testing " ++ name ++ " ... "
    let run (f,x) = let res = f x in do evaluate $ rnf res; return res
    a <- run orig
    putStr "... "
    b <- run opt
    when (a /= b) $ do
        error $ unlines ["","FATAL: Results do not match","WANTED:",show a,"GOT:",show b]
    putStrLn "success"


benchmarks :: [Benchmark] -> IO ()
benchmarks = defaultMain

benchmark :: NFData b => String -> (a -> b, a) -> (a -> b, a) -> Benchmark
benchmark name orig opt = bgroup name [bench "GHC" $ uncurry nf orig, bench "Supero" $ uncurry nf opt]


module Nofib(nofib) where

import Control.Monad
import Data.Maybe
import System.Cmd
import System.Directory
import System.FilePath

import Options


folders = ["imaginary","spectral","real"]

type Benchmark = String

--                                      errmsg executable
type Compiler = Options -> Benchmark -> IO (Either String String)


nofib :: Options -> [(String,Compiler)] -> [Benchmark] -> IO ()
nofib options comps benchs = do
    benchs <- resolveBenchmarks options benchs
    sequence_ [do
        putStrLn $ "Running " ++ takeBaseName b ++ " with " ++ name
        res <- c options b
        case res of
            Left err -> putStrLn $ "Doh: " ++ err
            Right exec -> runBenchmark (takeBaseName b) exec
        | b <- benchs, (name,c) <- comps]


resolveBenchmarks :: Options -> [Benchmark] -> IO [Benchmark]
resolveBenchmarks (Options {optNofibLocation=root}) bs = do
    let f x = liftM (filter ('.' `notElem`)) $ getDirectoryContents (root </> x)
    tests <- liftM (zip folders) $ mapM f folders

    let f "." = concatMap snd tests
        f x | x `elem` folders = fromJust $ lookup x tests
        f x = let res = [x | a <- concatMap snd tests, takeBaseName a == x]
              in if null res then error $ "Nofib test not found, " ++ x else res
    return $ concatMap f bs


runBenchmark :: Benchmark -> FilePath -> IO ()
runBenchmark bench exe = do
    system exe
    putStrLn "Done"

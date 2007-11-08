
module Nofib(nofib, Compiler, Benchmark) where

import Control.Monad
import Data.Maybe
import System.Cmd
import System.Directory
import System.FilePath
import Safe

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
        res <- c options{optObjLocation = optObjLocation options </> name </> takeBaseName b} b
        case res of
            Left err -> putStrLn $ "Doh: " ++ err
            Right exec -> runBenchmark (takeBaseName b) exec
        | b <- benchs, (name,c) <- comps]


resolveBenchmarks :: Options -> [Benchmark] -> IO [Benchmark]
resolveBenchmarks opts want = do
    found <- benchmarks opts
    return $ concatMap (`lookupJust` found) want


benchmarks :: Options -> IO [(String,[Benchmark])]
benchmarks (Options {optNofibLocation=root}) = do
        res <- mapM f folders
        return $ (".",concatMap snd $ concat res) : 
                 zipWith (\f r -> (f,concatMap snd r)) folders res ++
                 concat res
    where
        f folder = do
            res <- liftM (filter ('.' `notElem`)) $ getDirectoryContents (root </> folder)
            return $ map (\x -> (x,[root </> folder </> x])) res



runBenchmark :: Benchmark -> FilePath -> IO ()
runBenchmark bench exe = do
    system exe
    putStrLn "Done"



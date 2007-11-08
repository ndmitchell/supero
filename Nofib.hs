
module Nofib(nofib, Compiler, Benchmark) where

import Control.Monad
import Data.Maybe
import System.Cmd
import System.Directory
import System.FilePath
import System.Time
import Safe
import System.Info

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
            Right exec -> runBenchmark name b exec
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



runBenchmark :: String -> Benchmark -> FilePath -> IO ()
runBenchmark compiler bench exe = do
    let l = lookupJust (takeBaseName bench) tests
    r <- l bench exe
    case r of
        Left x -> putStrLn $ "Error:" ++ x
        Right x -> do
            when (compilerName /= "hugs") $
                appendFile "results.txt" (compiler ++ " " ++ show x ++ "\n")
            putStrLn $ "Time: " ++ show x


tests :: [(String, Benchmark -> FilePath -> IO (Either String Integer))]
tests =
    let a*b = (a,b) in
    ["bernouilli" * checked "500"
    ,"digits-of-e1" * checked "1000"
    ,"x2n1" * checked "10000"
    ]


checked :: String -> Benchmark -> FilePath -> IO (Either String Integer)
checked args bench exe = do
    let stdout = exe <.> "stdout"
        stderr = exe <.> "stderr"
    removeFileSafe stdout
    removeFileSafe stderr
    begin <- getClockTime
    system $ exe ++ " " ++ args ++ " > " ++ stdout ++ " 2> " ++ stderr
    end <- getClockTime
    let elapsed = diffMilliseconds end begin
    expected <- readFile (bench </> takeBaseName bench <.> "stdout")
    got <- readFile stdout
    return $ if got /= expected then Left "Expected mismatch" else Right elapsed


removeFileSafe x = do
    b <- doesFileExist x
    when b $ removeFile x


diffMilliseconds :: ClockTime -> ClockTime -> Integer
diffMilliseconds a b =
        fromIntegral (tdSec res * 1000) +
        fromIntegral (tdPicosec res `div` 1000000000)
    where res = diffClockTimes a b

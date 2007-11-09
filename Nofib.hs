
module Nofib(nofib, Compiler, Benchmark) where

import Control.Monad
import Data.Maybe
import Data.List
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
nofib opts comps benchs = do
    benchs <- resolveBenchmarks opts benchs
    sequence_ [do
        putStrLn $ "Running " ++ takeBaseName b ++ " with " ++ name
        let objdir = optObjLocation opts </> name </> takeBaseName b
            opts2 = opts{optObjLocation = objdir}
        createDirectoryIfMissing True objdir
        res <- c opts2 b
        case res of
            Left err -> putStrLn $ "Doh: " ++ err
            Right exec -> runBenchmark opts2 name b exec
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
            res <- getDirectoryContents (root </> folder)
            liftM concat $ mapM (g (root </> folder)) res

        g root x = do
            b <- doesDirectoryExist (root </> x)
            return [(x, [root </> x]) | b && '.' `notElem` x]


runBenchmark :: Options -> String -> Benchmark -> FilePath -> IO ()
runBenchmark opts compiler bench exe = do
    let l = fromMaybe (error $ "Don't know how to benchmark " ++ takeBaseName bench) $
                      lookup (takeBaseName bench) tests
    r <- l opts bench exe
    case r of
        Left x -> putStrLn $ "Error:" ++ x
        Right x -> do
            when (compilerName /= "hugs") $
                appendFile "results.txt" (compiler ++ " " ++ takeBaseName bench ++ " " ++ show x ++ "\n")
            putStrLn $ "Time: " ++ show x


tests :: [(String, Options -> Benchmark -> FilePath -> IO (Either String Integer))]
tests =
    let a*b = (a,b) in
    ["bernouilli" * checked "500"
    ,"digits-of-e1" * checked "1000"
    ,"digits-of-e2" * checked "2000"
    ,"exp3_8" * checked "8"
    ,"gen_regexps" * piped
    ,"integrate" * checked "50000"
    ,"paraffins" * checked "17"
    ,"primes" * checked "1500"
    ,"queens" * checked "10"
    ,"rfib" * checked "30"
    ,"tak" * checked "24 16 8"
    ,"wheel-sieve1" * checked "100000"
    ,"wheel-sieve2" * checked "20000"
    ,"x2n1" * checked "10000"
    ]


checked :: String -> Options -> Benchmark -> FilePath -> IO (Either String Integer)
checked args opts bench exe = do
    let logs = optObjLocation opts </> "runtime"
        stdout = logs <.> "stdout"
        stderr = logs <.> "stderr"
    removeFileSafe stdout
    removeFileSafe stderr
    begin <- getClockTime
    system $ exe ++ " " ++ args ++ " > " ++ stdout ++ " 2> " ++ stderr
    end <- getClockTime
    let elapsed = diffMilliseconds end begin
    expected <- readFile (bench </> takeBaseName bench <.> "stdout")
    got <- readFile stdout
    return $ if got /= expected then Left "Expected mismatch" else Right elapsed

piped :: Options -> Benchmark -> FilePath -> IO (Either String Integer)
piped opts bench exe = do
    let stdin = bench </> takeBaseName bench <.> "stdin"
    checked (" < " ++ stdin) opts bench exe


removeFileSafe x = do
    b <- doesFileExist x
    when b $ removeFile x


diffMilliseconds :: ClockTime -> ClockTime -> Integer
diffMilliseconds a b =
        fromIntegral (tdSec res * 1000) +
        fromIntegral (tdPicosec res `div` 1000000000)
    where res = diffClockTimes a b

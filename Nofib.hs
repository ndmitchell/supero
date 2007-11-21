
module Nofib(nofib, Nofib(..), Compiler, Benchmark) where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Char
import System.Directory
import System.FilePath
import System.Time
import System.Cmd
import Safe
import System.Info

import General


folders = ["imaginary","spectral","real"]


exclude = let (*) = (,) in
    ["integrate" * "supero-none" -- runs out of memory
    ,"paraffins" * "supero-none" -- requires Array primitives
    ]


type Benchmark = String

type Compiler = Options -> Benchmark -> IO Answer

data Nofib = Nofib
             {repetitions :: Int
             ,rebuild :: Bool
             }


nofib :: Options -> Nofib -> [(String,Compiler)] -> [Benchmark] -> IO ()
nofib opts Nofib{repetitions=repetitions, rebuild=rebuild} comps benchs = do
    benchs <- resolveBenchmarks opts benchs
    sequence_ [do
        putStrLn $ "Running " ++ takeBaseName b ++ " with " ++ name
        let objdir = optObjLocation opts </> name </> takeBaseName b
            opts2 = opts{optObjLocation = objdir}
            exec = objdir </> "main"
        createDirectoryIfMissing True objdir
        res <- if rebuild then c opts2 b else return Success
        case res of
            Failure err -> putStrLn $ "Doh: " ++ err
            Success -> replicateM_ repetitions $ runBenchmark opts2 name b exec
        | b <- benchs, (name,c) <- comps
        , (takeBaseName b, name) `notElem` exclude]


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
        Left x -> putStrLn $ "Error: " ++ x
        Right x -> do
            when (compilerName /= "hugs") $
                appendFile "results.txt" (compiler ++ " " ++ takeBaseName bench ++ " " ++ show x ++ "\n")
            putStrLn $ "Time: " ++ show x


tests :: [(String, Options -> Benchmark -> FilePath -> IO (Either String Integer))]
tests =
    let a*b = (a,b)
        noSpaces = filter (not . isSpace)
    in
    ["bernouilli" * checkedBy (\a b -> noSpaces a == noSpaces b) "500"
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


checked = checkedBy (==)

checkedBy :: (String -> String -> Bool) -> String -> Options -> Benchmark -> FilePath -> IO (Either String Integer)
checkedBy comp args opts bench exe = do
    let logs = optObjLocation opts </> "runtime"
        stdout = logs <.> "stdout"
        stderr = logs <.> "stderr"
    removeFileSafe stdout
    removeFileSafe stderr
    begin <- getClockTime
    system $ exe ++ " " ++ args ++ " > " ++ stdout ++ " 2> " ++ stderr
    end <- getClockTime
    let elapsed = diffMilliseconds end begin
    expected <- readFile' (bench </> takeBaseName bench <.> "stdout")
    got <- readFile' stdout
    return $ if got `comp` expected then Right elapsed
             else Left $ "Result wrong:\nExpected: " ++ expected ++ "\nGot: " ++ got ++ "\n"

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

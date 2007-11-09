
module Main where

import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.Directory
import System.FilePath

import General
import Nofib
import Report
import Optimise.All

compilers = [("yhc",runYhc)
            ,("ghc",runGHC "")
            ,("ghc1",runGHC "-O1")
            ,("ghc2",runGHC "-O2")
            ,("supero",runSupero)
            ]


main = do
    args <- getArgs
    let (cs,ts) = partition (`elem` map fst compilers) args
        comps = map (\c -> (c, fromJust $ lookup c compilers)) cs
    opts <- readOptions
    nofib opts comps ts
    report



runGHC :: String -> Options -> Benchmark -> IO (Either String String)
runGHC flag (Options {optObjLocation=obj}) bench = do
    let exe = obj </> "main.exe"
    b <- doesFileExist exe
    when (not b) $ do
        system_ $ "ghc --make " ++ (bench </> "Main") ++ " " ++ flag ++ " " ++
                  " -odir " ++ obj ++ " -hidir " ++ obj ++ " -o " ++ exe ++
                  "  > " ++ (obj </> "compile.stdout") ++
                  " 2> " ++ (obj </> "compile.stderr")
    b <- doesFileExist exe
    return $ if b then Right exe else Left "Could not create executable"


runYhc :: Options -> Benchmark -> IO (Either String String)
runYhc (Options {optObjLocation=obj}) bench = do
    let exe = obj </> "main.hbc"
    b <- doesFileExist exe
    when (not b) $ do
        system_ $ "yhc " ++ (bench </> "Main") ++
                  " --objdir=" ++ obj ++ " --hidir=" ++ obj ++
                  "  > " ++ (obj </> "compile.stdout") ++
                  " 2> " ++ (obj </> "compile.stderr")
    b <- doesFileExist exe
    return $ if b then Right ("yhi " ++ exe) else Left "Could not create executable"


runSupero :: Options -> Benchmark -> IO (Either String String)
runSupero (Options {optObjLocation=obj}) bench = do
    optimise bench obj

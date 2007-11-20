
module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Safe
import System.Environment
import System.Directory
import System.FilePath
import System.IO

import General
import Nofib
import Report
import Optimise.All

compilers = [("yhc",runYhc)
            ,("ghc",runGHC "")
            ,("ghc1",runGHC "-O1")
            ,("ghc2",runGHC "-O2")
            ]


main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    (nums,args) <- return $ partition (all isDigit) args
    (cs,args) <- return $ partition (`elem` map fst compilers) args
    (ts,args) <- return $ partition (`elem` map fst termination) args

    let comps = map (\c -> (c, lookupJust c compilers)) cs ++
                map (\c -> ("supero-" ++ c, runSupero $ lookupJust c termination)) ts

    opts <- readOptions
    nofib opts (headDef 1 (map read nums)) comps args
    report



runGHC :: String -> Options -> Benchmark -> IO Answer
runGHC flag (Options {optObjLocation=obj}) bench = do
    let exe = obj </> "main.exe"
    b <- doesFileExist exe
    when (not b) $
        system_ ("ghc --make " ++ (bench </> "Main") ++ " " ++ flag ++ " " ++
                 " -odir " ++ obj ++ " -hidir " ++ obj ++ " -o " ++ exe)
                (obj </> "compile.stdout")
                (obj </> "compile.stderr")
    b <- doesFileExist exe
    return $ if b then Success else Failure "Could not create executable"


runYhc :: Options -> Benchmark -> IO Answer
runYhc (Options {optObjLocation=obj}) bench = do
    let exe = obj </> "main.hbc"
    b <- doesFileExist exe
    when (not b) $
        system_ ("yhc " ++ (bench </> "Main") ++
                 " --objdir=" ++ obj ++ " --hidir=" ++ obj)
                (obj </> "compile.stdout")
                (obj </> "compile.stderr")
    b <- doesFileExist exe
    return $ error "todo, create a .bat file to run it"
    -- return $ if b then Right ("yhi " ++ exe) else Left "Could not create executable"


runSupero :: Termination -> Options -> Benchmark -> IO Answer
runSupero term (Options {optObjLocation=obj}) bench = do
    optimise term bench obj

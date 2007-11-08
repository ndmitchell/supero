
module Main where

import Data.List
import Data.Maybe
import System.Environment

import Options
import Nofib

compilers = [("yhc",undefined)
            ,("ghc",runGHC "")
            ,("ghc1",runGHC "-O1")
            ,("ghc2",runGHC "-O2")
            ,("supero",undefined)
            ]


main = do
    args <- getArgs
    let (cs,ts) = partition (`elem` map fst compilers) args
        comps = map (\c -> (c, fromJust $ lookup c compilers)) cs
    opts <- readOptions
    nofib opts comps ts





runGHC :: String -> Options -> Benchmark -> IO (Either String String)
runGHC flag opts bench = do
    error $ show (flag,opts,bench)



module Main where

import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.Directory
import System.FilePath
import System.Cmd

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
runGHC flag (Options {optObjLocation=obj}) bench = do
    let exe = obj </> "main.exe"
    b <- doesFileExist exe
    when (not b) $ do
        createDirectoryIfMissing True obj
        system $ "ghc --make " ++ (bench </> "Main") ++ " " ++ flag ++ " " ++
                 " -odir " ++ obj ++ " -hidir " ++ obj ++ " -o " ++ exe ++
                 "  > " ++ (obj </> "compile.stdout") ++
                 " 2> " ++ (obj </> "compile.stderr")
        return ()
    b <- doesFileExist exe
    return $ if b then Right exe else Left "Could not create executable"


module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Environment

import Compiler.All
import Core.All


main = do
    xs <- getArgs
    xs <- if xs /= [] then return $ map (++".hs") xs else
          liftM (filter (".hs" `isSuffixOf`)) $ getDirectoryContents "Tests"
    forM_ xs $ \x -> do
        putStrLn $ "Compiling " ++ x
        let src = "Tests/" ++ x
        writeCore (src ++ ".txt") . compile =<< readCore src
    putStrLn "Done"

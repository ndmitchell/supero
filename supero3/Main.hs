
module Main where

import Desugar
import Supercompile
import Type
import Simplify

import Control.Monad
import Language.Haskell.Exts
import System.Environment
import System.FilePath
import Data.List
import System.Cmd
import System.Directory
import System.Exit
import Control.Exception
import Language.Preprocessor.Cpphs
import System.IO.Unsafe


main = do
    xs <- getArgs
    let (opts,files) = partition ("-" `isPrefixOf`) xs
    forM_ files $ \x -> do
        let y = dropExtension x <.> "opt.hs"
        src <- readFile x
        let res = fleshOut src $ prettyPrint $ toHSE $ supercompile $ env $ simplifyProg $ fromHSE $ desugar $
                        fromParseResult $ parseFileContents $ cpphs ["SUPERO"] src
        writeFile y res
        when ("--compile" `elem` opts) $ do
            withDirectory (takeDirectory x) $ do
                system_ $ "ghc --make -O2 " ++ takeFileName x ++ " -ddump-simpl -cpp -DMAIN -DMAIN_GHC > " ++ takeFileName x ++ ".log"
                system_ $ "ghc --make -O2 " ++ takeFileName y ++ " -ddump-simpl > " ++ takeFileName y ++ ".log"


-- not unsafe since no include files
cpphs :: [String] -> String -> String
cpphs defs = unsafePerformIO . runCpphs defaultCpphsOptions{defines=map (flip (,) "1") defs} ""


withDirectory new act = do
    old <- getCurrentDirectory
    bracket_
        (setCurrentDirectory new)
        (setCurrentDirectory old)
        act


system_ cmd = do
    putStrLn cmd
    res <- system cmd
    when (res /= ExitSuccess) $ error "system command failed"


fleshOut :: String -> String -> String
fleshOut orig new = "{-# OPTIONS_GHC -O2 #-}\nmodule Main(main) where\n" ++ f "MAIN" ++ f "MAIN_SUPERO" ++ new ++ "\n\n"
    where f x = unlines $ takeWhile (/= "#endif") $ drop 1 $ dropWhile (/= ("#if " ++ x)) $ lines orig


module Main where

import Supercompile
import Exp
import Simplify
import Util

import Control.Applicative
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
    args <- getArgs
    when (null args) $ do
        putStrLn $ "Arguments: --compile --test --benchmark [FILE|DIR]"
        exitSuccess
    let (opts,files) = partition ("-" `isPrefixOf`) args
    files <- findFiles files
    createDirectoryIfMissing True "obj"
    let modu = intercalate "." . splitDirectories . dropExtension
    forM_ files $ \inp -> do
        let out = dropExtension inp ++ "_gen.hs"
        src <- readFile inp
        let res = fleshOut (modu out) src $ prettyPrint $ toHSE $ supercompile $ simplifys $ fromHSE $
                        fromParseResult $ parseFileContents $ cpphs ["SUPERO"] src
        timer $ writeFile out res
        when ("--compile" `elem` opts) $ do
            createDirectoryIfMissing True $ "obj" </> takeDirectory out
            timer $ system_ $ "ghc -O2 " ++ out ++ " -ddump-simpl -outputdir obj > obj/" ++ out ++ ".core"

    when ("--test" `elem` opts) $ do
        createDirectoryIfMissing True "obj"
        let ms = map modu files
        writeFile "Test_gen.hs" $ unlines $
            ["module Test_gen(main) where"
            ,"import Support"] ++
            ["import qualified " ++ m ++ "; import qualified " ++ m ++ "_gen" | m <- ms] ++
            ["main = do"] ++
            ["    testEqual \"" ++ m ++ "\" " ++ m ++ ".main " ++ m ++ "_gen.main" | m <- ms]
        system_ $ "ghc -O2 --make Test_gen.hs -outputdir obj -XCPP -DMAIN -o obj/Test_gen.exe -main-is Test_gen.main"
        system_ $ "obj" </> "Test_gen.exe"

    when ("--benchmark" `elem` opts) $ do
        putStrLn "benchmark here"


-- safe since no include files
cpphs :: [String] -> String -> String
cpphs defs = unsafePerformIO . runCpphs defaultCpphsOptions{defines=map (flip (,) "1") defs} ""


findFiles :: [String] -> IO [FilePath]
findFiles want = do
    xs <- filter ((==) ".hs" . takeExtension) <$> getDirectoryContentsRecursive ""
    return $ filter (\x -> any (`elem` map (lower . dropExtension) (splitDirectories x)) $ map lower want) xs


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


fleshOut :: String -> String -> String -> String
fleshOut modu orig new =
    "{-# OPTIONS_GHC -O2 #-}\nmodule " ++ modu ++ "(main) where\n" ++
    f "IMPORT_SUPERO" ++ f "MAIN" ++ f "MAIN_SUPERO" ++ new ++ "\n\n"
    where f x = unlines $ takeWhile (/= "#endif") $ drop 1 $ dropWhile (/= ("#if " ++ x)) $ lines orig

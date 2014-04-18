
module Main where

import Supercompile
import Exp
import HSE
import Simplify
import Util
-- import Support() -- just to ensure it does compile

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

works = words "simple peter other digits_of_e2 exp3_8 rfib tak x2n1"

main = do
    args <- getArgs
    when (null args) $ do
        putStrLn $ "Arguments: --compile --test --benchmark [FILE|DIR]"
        exitSuccess
    let (opts,files) = partition ("-" `isPrefixOf`) args
    files <- findFiles $ (if "--work" `elem` opts then works else []) ++ files
    createDirectoryIfMissing True "obj"
    let modu = intercalate "." . splitDirectories . dropExtension
    forM_ files $ \inp -> do
        putStrLn $ "Converting " ++ inp
        let out = dropExtension inp ++ "_gen.hs"
        src <- readFile inp
        let res = fleshOut (modu out) src $ prettyPrint $ noCAF $ toHSE $ supercompile $ simplifys $ fromHSE $
                        fromParseResult $ parseFileContents $ cpphs ["SUPERO"] src
        timer $ writeFile out res
        when ("--compile" `elem` opts || "--test" `elem` opts || "--benchmark" `elem` opts) $ do
            createDirectoryIfMissing True $ "obj" </> takeDirectory out
            timer $ system_ $ "ghc " ++ opt ++ " " ++ out ++ " -ddump-simpl -outputdir obj > obj/" ++ out ++ ".core"

    let execute fun args = do
        createDirectoryIfMissing True "obj"
        let ms = map modu files
        writeFile ("obj/" ++ fun ++ "_gen.hs") $ unlines $
            ["module " ++ fun ++ "_gen(main) where"
            ,"import Support"] ++
            ["import qualified " ++ m ++ "; import qualified " ++ m ++ "_gen" | m <- ms] ++
            ["main = " ++ lower fun ++ "s"] ++
            ["    " ++ (if i == 0 then "[" else ",") ++ lower fun ++ " \"" ++ m ++ "\" " ++ m ++ ".test " ++ m ++ "_gen.test"
                | (i,m) <- zip [0..] ms] ++
            ["    " ++ ['[' | null ms] ++ "]"]
        system_ $ "ghc " ++ opt ++ " -rtsopts --make obj/" ++ fun ++ "_gen.hs -outputdir obj -XCPP -DMAIN -I. -o obj/" ++ fun ++ "_gen.exe -main-is " ++ fun ++ "_gen.main"
        system_ $ "obj" </> fun ++ "_gen.exe +RTS -K20M -RTS " ++ args

    when ("--test" `elem` opts) $
        execute "Test" ""
    when ("--benchmark" `elem` opts) $ do
        execute "Benchmark" "-oreport.html -ureport.csv"
        src <- lines <$> readFile' "report.csv"
        let grab s = head [read $ takeWhile (/= ',') x :: Double | x <- src, Just x <- [stripPrefix ("\"" ++ s ++ "\",") x]]
        forM_ (map modu files) $ \m ->
            putStrLn $ m ++ " = " ++ show (grab (m ++ "/Supero") / grab (m ++ "/GHC"))

opt = "-O2 -fno-full-laziness"

-- safe since no include files
cpphs :: [String] -> String -> String
cpphs defs = unsafePerformIO . runCpphs opts ""
    where opts = defaultCpphsOptions{defines=map (flip (,) "1") defs, boolopts=defaultBoolOptions{locations=False}}

findFiles :: [String] -> IO [FilePath]
findFiles want = do
    xs <- filter (\x -> not ("_gen.hs" `isSuffixOf` x) && takeExtension x == ".hs") . map (drop 2) <$> getDirectoryContentsRecursive "."
    return $ filter (\x -> any (`elem` map (lower . dropExtension) (splitDirectories x)) $ map lower want) xs

fleshOut :: String -> String -> String -> String
fleshOut modu orig new =
    "{-# LANGUAGE UnboxedTuples #-}\n" ++
    "module " ++ modu ++ "(test) where\n" ++
    f "IMPORT_SUPERO" ++ f "MAIN" ++ f "MAIN_SUPERO" ++ new ++ "\n\n"
    where f x = unlines $ takeWhile (/= "#endif") $ drop 1 $ dropWhile (/= ("#if " ++ x)) $ lines orig

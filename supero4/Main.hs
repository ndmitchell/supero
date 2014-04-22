
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
import System.Directory
import System.Exit
import Language.Preprocessor.Cpphs
import System.IO.Unsafe

{-
Current thoughts about where jail is necessary:

* Accumulating parameter.
* Even with primities (X2n1 with accumulating sum requires a primitive)
* Duplicating recursive call (see exp3_8)?
* Recursive call underneath a non-primitive?

Next step:

* Pull out a few examples with minimal uses of jails, including
  - exp3_8 &* requires a jail
  - x2n1 with sum accumulating requires a jail
  - reverse requires a jail

Anything to the left or right of a primitive, or the result of a primitive, can be jailed
without loss of information (these things don't generate any info anyway)
-}



works = words "simple peter other jail digits_of_e2 digits_of_e1 exp3_8 rfib tak x2n1 primes gen_regexps queens integrate bernouilli paraffins"
    -- wheel_sieve1 - unconverted (CAFs and list comp)
    -- wheel_sieve2 - unconverted (CAFs and letrec)

main = do
    args <- getArgs
    when (null args) $ do
        putStrLn $ "Arguments: --compile --test --benchmark [FILE|DIR]"
        exitSuccess
    let (opts,files) = partition ("-" `isPrefixOf`) args
    files <- findFiles $ (if "--work" `elem` opts then works else []) ++ files
    files <- return $ if "--nofib" `notElem` opts then files else filter ("Nofib" `isInfixOf`) files
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
            timer $ system_ $ "ghc -XCPP -DMAIN -I. " ++ opt ++ " " ++ inp ++ " -ddump-simpl -outputdir obj > obj/" ++ inp ++ ".core"

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
            putStrLn $ m ++ " = " ++ showDP 2 (grab (m ++ "/Supero") / grab (m ++ "/GHC"))

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
    "{-# LANGUAGE UnboxedTuples, NoMonomorphismRestriction #-}\n" ++
    "module " ++ modu ++ "(test) where\n" ++
    f "IMPORT_SUPERO" ++ f "MAIN" ++ f "MAIN_SUPERO" ++ new ++ "\n\n"
    where f x = unlines $ takeWhile (/= "#endif") $ drop 1 $ dropWhile (/= ("#if " ++ x)) $ lines orig

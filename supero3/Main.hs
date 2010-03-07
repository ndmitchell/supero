
module Main where

import Desugar
import Supercompile
import Type
import Simplify

import Control.Monad
import Language.Haskell.Exts
import System.Environment
import System.FilePath


main = do
    xs <- getArgs
    let opt = prettyPrint . toHSE . supercompile . env . simplifyProg . fromHSE . desugar
    forM_ xs $ \x -> do
        src <- fmap fromParseResult $ parseFile x
        writeFile (dropExtension x <.> "opt.hs") $ opt src

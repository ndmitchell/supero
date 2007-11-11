
module General(
    Options(..), readOptions,
    system_
    ) where

import System.Directory
import Control.Monad
import Safe
import System.Cmd
import System.Exit


data Options = Options {
                   optNofibLocation :: FilePath,
                   optObjLocation :: FilePath
               }
               deriving Show


readOptions :: IO Options
readOptions = do
    nofib <- locate ["C:\\Documents\\Uni\\nofib","D:\\sources\\contrib\\nofib"]
    obj <- locate ["F:\\Temp\\supero","D:\\Temp\\supero"]
    return $ Options nofib obj


locate opts = do
    liftM
        (headNote $ "Can't find a directory, wanted one of: " ++ show opts)
        (filterM doesDirectoryExist opts)


system_ cmd = do
    putStr $ "Running " ++ head (words cmd) ++ "... "
    res <- system cmd
    putStrLn "done"
    when (res /= ExitSuccess) $ error $ "ERROR: System call failed\n" ++ cmd


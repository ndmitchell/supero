
module General(
    Options(..), readOptions,
    system_, readFile',
    haskellFile, recompile
    ) where

import System.Directory
import System.FilePath
import Control.Monad
import Safe
import System.Cmd
import System.Exit
import System.IO


data Options = Options {
                   optNofibLocation :: FilePath,
                   optObjLocation :: FilePath
               }
               deriving Show


readOptions :: IO Options
readOptions = do
    nofib <- locate ["C:\\Documents\\Uni\\nofib","C:\\Neil\\nofib","D:\\sources\\contrib\\nofib"]
    obj <- locate ["F:\\Temp\\supero","D:\\Temp\\supero","C:\\Neil\\Temp\\supero"]
    return $ Options nofib obj


locate opts = do
    liftM
        (headNote $ "Can't find a directory, wanted one of: " ++ show opts)
        (filterM doesDirectoryExist opts)


system_ cmd stdout stderr = do
    putStr $ "Running " ++ head (words cmd) ++ "... "
    res <- system $ cmd ++ " > " ++ stdout ++ " 2> " ++ stderr
    putStrLn "done"
    when (res /= ExitSuccess) $ do
        out <- readFile stdout
        err <- readFile stderr
        error $ "ERROR: System call failed\n" ++ cmd ++ "\n" ++ out ++ "\n" ++ err

readFile' :: FilePath -> IO String
readFile' file = do
    h <- openFile file ReadMode
    s <- hGetContents h
    length s `seq` hClose h
    return s


recompile :: FilePath -> FilePath -> IO Bool
recompile from to = do
    b <- doesFileExist to
    if not b then return True else do
        f <- getModificationTime from
        t <- getModificationTime to
        return $ f > t


haskellFile :: FilePath -> IO FilePath
haskellFile s = do
    hs <- doesFileExist (s <.> "hs")
    lhs <- doesFileExist (s <.> "lhs")
    if hs then return $ s <.> "hs"
     else if lhs then return $ s <.> "lhs"
     else error $ "Haskell file not found: " ++ s

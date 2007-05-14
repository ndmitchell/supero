{-# OPTIONS_GHC -fffi #-}

import Foreign.C.Types
import System.IO
import System.IO.Unsafe


main = print . length =<< getContents2

foreign import ccall safe "stdio.h getchar" getchar :: IO CInt


getContents2 :: IO String
getContents2 = hGetContents2 stdin

hGetContents2 :: Handle -> IO String
hGetContents2 h = do
    c <- getchar
    if c == (-1) then return [] else do
        cs <- unsafeInterleaveIO $ hGetContents2 h
        return (toEnum (fromIntegral c) :cs)


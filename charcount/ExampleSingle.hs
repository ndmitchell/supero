{-# OPTIONS_GHC -fffi #-}

import Foreign.C.Types
import System.IO
import System.IO.Unsafe


main = print . length =<< getContents2

foreign import ccall safe "stdio.h getchar" getchar :: IO CInt


getContents2 :: IO String
getContents2 = do
    c <- getchar
    if c == (-1) then return [] else do
        cs <- unsafeInterleaveIO getContents2
        return (toEnum (fromIntegral c) :cs)


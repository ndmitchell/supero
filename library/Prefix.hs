{-# OPTIONS_GHC -fffi #-}

import System.IO.Unsafe
import System.IO
import Data.Word

main = main_main `seq` (return () :: IO ())

{-# NOINLINE wrapIO #-}
wrapIO x = unsafePerformIO (x >>= return . Overlay_IO)

system_IO_hPutChar h c = wrapIO (hPutChar h (toEnum c))
system_IO_hGetChar h   = wrapIO (getCharIO h)

foreign import ccall safe "stdio.h getchar" getchar :: IO Word8

{-# NOINLINE getCharIO #-}
getCharIO h = do
    c <- getchar
    return $ if c == (-1) then 0 else chr_ c

prelude_seq = seq

prelude_error x = error (map toEnum x)

aDD_W = (+) :: Int -> Int -> Int
eQ_W = (==) :: Int -> Int -> Bool
gT_W = (>) :: Int -> Int -> Bool
lT_W = (<) :: Int -> Int -> Bool
qUOT = quot :: Int -> Int -> Int
rEM = rem :: Int -> Int -> Int

nEG_W = negate :: Int -> Int


int_ x = x :: Int
chr_ x = fromEnum x
str_ x = map chr_ x


system_IO_stdin = stdin
system_IO_stdout = stdout

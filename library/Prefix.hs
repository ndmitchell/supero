{-# OPTIONS_GHC -fffi #-}

import System.IO.Unsafe
import System.IO
import Foreign.C.Types

main = main_main `seq` (return () :: IO ())

overlay_put_char h c = unsafePerformIO (hPutChar h (toEnum c) >> return 0)
overlay_get_char h   = unsafePerformIO (getCharIO h)

foreign import ccall safe "stdio.h getchar" getchar :: IO CInt

{-# NOINLINE getCharIO #-}
getCharIO h = do
   c <- getchar
   return $ if c == (-1) then h `seq` (-1) else fromIntegral c

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

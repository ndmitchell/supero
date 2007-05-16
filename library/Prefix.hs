{-# OPTIONS_GHC -fffi -fglasgow-exts -cpp #-}

import System.IO.Unsafe
import System.IO
import Foreign.C.Types


-- BEGIN Stolen from Data.ByteString.Base
#if defined(__GLASGOW_HASKELL__)
import GHC.Base                 (realWorld#)
import GHC.IOBase               (IO(IO), unsafePerformIO)
#else
import System.IO.Unsafe         (unsafePerformIO)
#endif

{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif
-- END


main = main_generated `seq` (return () :: IO ())

overlay_put_char h c = inlinePerformIO (hPutChar h (toEnum c) >> return 0)
overlay_get_char h   = inlinePerformIO (getCharIO h)

foreign import ccall unsafe "stdio.h getchar" getchar :: IO CInt

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

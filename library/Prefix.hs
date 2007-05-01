
import System.IO.Unsafe
import System.IO

main = main_main `seq` (return () :: IO ())

wrapIO x = unsafePerformIO (x >>= return . Overlay_IO)

p_System_IO_hPutChar h c = wrapIO (hPutChar h (toEnum c))
p_System_IO_hGetChar h   = wrapIO $ do
    b <- hIsEOF h
    if b then return 0 else hGetChar h >>= return . chr_

p_Prelude_seq = seq

p_Prelude_error x = error (map toEnum x)

p_ADD_W = (+) :: Int -> Int -> Int
p_EQ_W = (==) :: Int -> Int -> Bool
p_GT_W = (>) :: Int -> Int -> Bool
p_LT_W = (<) :: Int -> Int -> Bool
p_QUOT = quot :: Int -> Int -> Int
p_REM = rem :: Int -> Int -> Int

p_NEG_W = negate :: Int -> Int


int_ x = x :: Int
chr_ x = fromEnum x
str_ x = map chr_ x


p_System_IO_stdin = stdin
p_System_IO_stdout = stdout


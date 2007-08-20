{-# OPTIONS_GHC -fffi -fglasgow-exts -cpp #-}

module Main(main) where

import System.IO.Unsafe
import System.IO
import System.Environment
import Foreign.C.Types


import GHC.Base                 (realWorld#)
import GHC.IOBase               (IO(IO), unIO, unsafePerformIO)
import GHC.Prim                 (State#,RealWorld)

main = IO main_generated

overlay_get_char = unIO $ do
    c <- getchar
    return $ fromIntegral c

system_IO_hPutChar h c = unIO $ hPutChar h (toEnum c)

foreign import ccall unsafe "stdio.h getchar" getchar :: IO CInt
foreign import ccall unsafe "ctype.h iswspace" isspace :: CInt -> CInt

typeRealWorld :: State# RealWorld -> State# RealWorld
typeRealWorld x = x

prelude_seq = seq

prelude_error x = error (map toEnum x)

aDD_W = (+) :: Int -> Int -> Int
sUB_W = (-) :: Int -> Int -> Int
eQ_W = (==) :: Int -> Int -> Bool
nE_W = (/=) :: Int -> Int -> Bool
gT_W = (>) :: Int -> Int -> Bool
lT_W = (<) :: Int -> Int -> Bool
lE_W = (<=) :: Int -> Int -> Bool
qUOT = quot :: Int -> Int -> Int
rEM = rem :: Int -> Int -> Int
nEG_W = negate :: Int -> Int


int_ x = x :: Int
chr_ x = fromEnum x
str_ x = map chr_ x


system_IO_stdin = stdin
system_IO_stdout = stdout

system_Environment_getArgs :: State# RealWorld -> (# State# RealWorld, [[Int]] #)
system_Environment_getArgs r = case (unIO getArgs) r of
                                (# r, s #) -> (# r, map str_ s #)

data_Char_isSpace c = isspace (toEnum c) /= 0


prelude_Int_Read_readsPrec :: Int -> ReadS Int
prelude_Int_Read_readsPrec p s = [(a, str_ b) | (a,b) <- readsPrec p (map toEnum s)]


{- OLD PREFIX

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

main = main_generated `seq` (return () :: IO ())

overlay_get_char = unIO $ do
    c <- getchar
    return $ fromIntegral c

overlay_token = 0 :: Int

overlay_put_char h c = inlinePerformIO (hPutChar h (toEnum c) >>
overlay_get_char h   = inlinePerformIO (getCharIO h)

-# NOINLINE getCharIO #-
getCharIO h = do
   c <- getchar
   return $ if c == (-1) then h `seq` (-1) else fromIntegral c
-}

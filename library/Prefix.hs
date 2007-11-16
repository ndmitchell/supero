{-# OPTIONS_GHC -fffi -fglasgow-exts -cpp -pgmPcpphs -optP--cpp #-}

module Main(main) where

import System.IO.Unsafe
import System.IO
import System.Environment
import System.Exit
import Foreign.C.Types


import GHC.Base                 (realWorld#)
import GHC.IOBase               (IO(IO), unIO, unsafePerformIO)
import GHC.Prim                 (State#,RealWorld)

main = IO main_generated

overlay_errorIO :: [Int] -> State# RealWorld -> (# State# RealWorld, a #)
overlay_errorIO x r = case unIO (putStrLn ("ERROR: " ++ map toEnum x)) r of
                           (# r, _ #) -> unIO exitFailure r

overlay_get_char = unIO $ do
    c <- getchar
    return $ fromIntegral c

system_IO_hPutChar h c = unIO $ hPutChar h (toEnum c)

foreign import ccall unsafe "stdio.h getchar" getchar :: IO CInt
foreign import ccall unsafe "ctype.h iswspace" isspace :: CInt -> CInt

typeRealWorld :: State# RealWorld -> State# RealWorld
typeRealWorld x = x

skipCAF :: State# RealWorld -> a -> a
skipCAF _ x = x

{-
-- Not allowed by GHC, so do the inlining in Optimise.CAF
argCAF :: State# RealWorld
argCAF = realWorld#
-}
#define argCAF realWorld#

prelude_seq = seq

prelude_error x = error (map toEnum x)

aDD_W = (+) :: Int -> Int -> Int
mUL_W = (*) :: Int -> Int -> Int
sUB_W = (-) :: Int -> Int -> Int
eQ_W = (==) :: Int -> Int -> Bool
nE_W = (/=) :: Int -> Int -> Bool
gT_W = (>) :: Int -> Int -> Bool
gE_W = (>=) :: Int -> Int -> Bool
lT_W = (<) :: Int -> Int -> Bool
lE_W = (<=) :: Int -> Int -> Bool
qUOT = quot :: Int -> Int -> Int
rEM = rem :: Int -> Int -> Int
nEG_W = negate :: Int -> Int
yHC_Primitive_primIntAbs = abs :: Int -> Int
yHC_Primitive_primIntSignum = signum :: Int -> Int
yHC_Primitive_primIntegerAdd = (+) :: Integer -> Integer -> Integer
yHC_Primitive_primIntegerEq = (==) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerFromInt = toInteger :: Int -> Integer
yHC_Primitive_primIntegerGe = (>=) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerGt = (>) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerLe = (<=) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerMul = (*) :: Integer -> Integer -> Integer
yHC_Primitive_primIntegerNe = (/=) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerNeg = negate :: Integer -> Integer
yHC_Primitive_primIntegerQuot = quot :: Integer -> Integer -> Integer
yHC_Primitive_primIntegerQuotRem = quotRem :: Integer -> Integer -> (Integer, Integer)
yHC_Primitive_primIntegerRem = rem :: Integer -> Integer -> Integer
yHC_Primitive_primIntFromInteger = fromInteger :: Integer -> Int
yHC_Primitive_primIntegerLt = (<) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerSub = (-) :: Integer -> Integer -> Integer


aDD_D = (+) :: Double -> Double -> Double
sUB_D = (-) :: Double -> Double -> Double
lT_D = (<) :: Double -> Double -> Bool
lE_D = (<=) :: Double -> Double -> Bool
gT_D = (>) :: Double -> Double -> Bool
gE_D = (>=) :: Double -> Double -> Bool

-- things which Yhc decides should be hopelessly slow
prelude_Int_Integral_mod = mod :: Int -> Int -> Int
prelude_Integer_Integral_div = div :: Integer -> Integer -> Integer
prelude_Integer_Integral_mod = mod :: Integer -> Integer -> Integer
prelude_Integer_Num_signum = signum :: Integer -> Integer
prelude_Integer_Num_abs = abs :: Integer -> Integer


int_ x = x :: Int
chr_ x = fromEnum x
str_ x = map chr_ x


system_IO_stdin = stdin
system_IO_stdout = stdout

system_Environment_getArgs :: State# RealWorld -> (# State# RealWorld, [[Int]] #)
system_Environment_getArgs r = case (unIO getArgs) r of
                                (# r, s #) -> (# r, map str_ s #)

data_Char_isSpace c = isspace (toEnum c) /= 0



type ReadsPrec a = Int -> [Int] -> [(a,[Int])]


prelude_Int_Read_readsPrec :: ReadsPrec Int
prelude_Int_Read_readsPrec p s = [(a, str_ b) | (a,b) <- readsPrec p (map toEnum s)]

prelude_Integer_Read_readsPrec :: ReadsPrec Integer
prelude_Integer_Read_readsPrec p s = [(a, str_ b) | (a,b) <- readsPrec p (map toEnum s)]

prelude_Double_Read_readsPrec :: ReadsPrec Double
prelude_Double_Read_readsPrec p s = [(a, str_ b) | (a,b) <- readsPrec p (map toEnum s)]

prelude_Char_Read_readsPrec :: ReadsPrec Int
prelude_Char_Read_readsPrec p s = [(chr_ (a :: Char), str_ b) | (a,b) <- readsPrec p (map toEnum s)]

prelude_Char_Show_showList :: [Int] -> [Int] -> [Int]
prelude_Char_Show_showList value rest = str_ (show (map toEnum value :: [Char])) ++ rest

prelude_Int_Show_showsPrec :: Int -> Int -> [Int] -> [Int]
prelude_Int_Show_showsPrec prec i rest = str_ (showsPrec prec i []) ++ rest

prelude_Integer_Show_showsPrec :: Int -> Integer -> [Int] -> [Int]
prelude_Integer_Show_showsPrec prec i rest = str_ (showsPrec prec i []) ++ rest

prelude_Double_Show_showsPrec :: Int -> Double -> [Int] -> [Int]
prelude_Double_Show_showsPrec prec i rest = str_ (showsPrec prec i []) ++ rest


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

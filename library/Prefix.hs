{-# OPTIONS_GHC -fffi -fglasgow-exts -cpp -pgmPcpphs -optP--cpp #-}

module Main(main) where

import System.IO.Unsafe
import System.IO
import System.Environment
import System.Exit
import Foreign.C.Types
import Data.Char(ord,chr)
import Data.Array


-- low level imports
import GHC.Base                 (realWorld#)
import GHC.IOBase               (IO(IO), unIO, unsafePerformIO)
import GHC.Prim                 (State#,RealWorld)


-- FFI replacements for Haskell stuff
foreign import ccall unsafe "stdio.h getchar" getchar :: IO CInt
foreign import ccall unsafe "ctype.h iswspace" isspace :: CInt -> CInt


-- CAF removal stuff
{-
-- Not allowed by GHC, so use the CPP
argCAF :: State# RealWorld
argCAF = realWorld#
-}
#define argCAF realWorld#

skipCAF :: State# RealWorld -> a -> a
skipCAF _ x = x


-- IO Subsystem
-- Unboxed IO is more efficient, but requires a certain level of
-- optimisation, so provide a BOXED_IO fallback

#if 1 || defined(BOXED_IO)

data RW_Box = RW_Box (State# RealWorld)
type RW_Pair a = (RW_Box, a)

fromIO :: IO a -> (RW_Box -> RW_Pair a)
fromIO a (RW_Box r) = case unIO a r of (# r, x #) -> (RW_Box r, x)

toIO :: (RW_Box -> RW_Pair a) -> IO a
toIO f = IO $ \r -> case f (RW_Box r) of (RW_Box r, x) -> (# r, x #)

#define PAIR_WORLD0     (,)
#define PAIR_WORLD(a,b) (a, b)
#define WORLD (RW_Box realWorld#)

#else

type RW_Box = State# RealWorld
type RW_Pair a = (# RW_Box, a #)

fromIO :: IO a -> (RW_Box -> RW_Pair a)
fromIO = unIO

toIO :: (RW_Box -> RW_Pair a) -> IO a
toIO = IO

#define PAIR_WORLD0 (error "INVALID, PAIR_WORLD0 disallowed with unboxed IO")
#define PAIR_WORLD(a,b) (# a :: State# RealWorld, b #)
#define WORLD realWorld#

#endif


-- IO functions not dependent on the IO primitives
main :: IO ()
main = toIO main_generated

typeRealWorld :: RW_Box -> RW_Box
typeRealWorld x = x

overlay_get_char :: RW_Box -> RW_Pair Int
overlay_get_char = fromIO $ do
    c <- getchar
    return $ fromIntegral c

system_IO_hPutChar :: Handle -> Int -> RW_Box -> RW_Pair ()
system_IO_hPutChar h c = fromIO $ hPutChar h (chr c)

overlay_errorIO :: [Int] -> RW_Box -> RW_Pair a
overlay_errorIO x r = case fromIO (putStrLn ("ERROR: " ++ map chr x)) r of
                           PAIR_WORLD(r, _) -> fromIO exitFailure r

system_Environment_getArgs :: RW_Box -> RW_Pair [[Int]]
system_Environment_getArgs r = case (fromIO getArgs) r of
                                    PAIR_WORLD(r, s) -> PAIR_WORLD(r, map str_ s)

overlay_supero_wrap x = x


-- Primitives
prelude_seq = seq

prelude_error x = error (map chr x)

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
eQ_D = (==) :: Double -> Double -> Bool
mUL_D = (*) :: Double -> Double -> Double
nEG_D = (negate) :: Double -> Double
nE_D = (/=) :: Double -> Double -> Bool
sLASH_D = (/) :: Double -> Double -> Double
yHC_Primitive_primDecodeDouble = decodeFloat :: Double -> (Integer,Int)
yHC_Primitive_primDoubleACos = acos :: Double -> Double
yHC_Primitive_primDoubleASin = asin :: Double -> Double
yHC_Primitive_primDoubleATan = atan :: Double -> Double
yHC_Primitive_primDoubleAbs = abs :: Double -> Double
yHC_Primitive_primDoubleCos = cos :: Double -> Double
yHC_Primitive_primDoubleExp = exp :: Double -> Double
yHC_Primitive_primDoubleFromInteger = fromInteger :: Integer -> Double
yHC_Primitive_primDoubleLog = log :: Double -> Double
yHC_Primitive_primDoublePow = (**) :: Double -> Double -> Double
yHC_Primitive_primDoubleSignum = signum :: Double -> Double
yHC_Primitive_primDoubleSin = sin :: Double -> Double
yHC_Primitive_primDoubleSqrt = sqrt :: Double -> Double
yHC_Primitive_primDoubleTan = tan :: Double -> Double
yHC_Primitive_primEncodeDouble = encodeFloat :: Integer -> Int -> Double




-- things which Yhc decides should be hopelessly slow
prelude_Int_Integral_mod = mod :: Int -> Int -> Int
prelude_Integer_Integral_div = div :: Integer -> Integer -> Integer
prelude_Integer_Integral_mod = mod :: Integer -> Integer -> Integer
prelude_Integer_Num_signum = signum :: Integer -> Integer
prelude_Integer_Num_abs = abs :: Integer -> Integer


int_ x = x :: Int
chr_ x = ord x
str_ x = map chr_ x


system_IO_stdin = stdin
system_IO_stdout = stdout

data_Char_isSpace :: Int -> Bool
data_Char_isSpace c = isspace (toEnum c) /= 0



type ReadsPrec a = Int -> [Int] -> [(a,[Int])]


prelude_Int_Read_readsPrec :: ReadsPrec Int
prelude_Int_Read_readsPrec p s = [(a, str_ b) | (a,b) <- readsPrec p (map chr s)]
prelude_Int_Read_readList = undefined

prelude_Integer_Read_readsPrec :: ReadsPrec Integer
prelude_Integer_Read_readsPrec p s = [(a, str_ b) | (a,b) <- readsPrec p (map chr s)]
prelude_Integer_Read_readList = undefined

prelude_Double_Read_readsPrec :: ReadsPrec Double
prelude_Double_Read_readsPrec p s = [(a, str_ b) | (a,b) <- readsPrec p (map chr s)]
prelude_Double_Read_readList = undefined

prelude_Char_Read_readsPrec :: ReadsPrec Int
prelude_Char_Read_readsPrec p s = [(chr_ (a :: Char), str_ b) | (a,b) <- readsPrec p (map chr s)]

prelude_Char_Show_showList :: [Int] -> [Int] -> [Int]
prelude_Char_Show_showList value rest = str_ (show (map chr value)) ++ rest

prelude_Char_Show_showsPrec :: Int -> Int -> [Int] -> [Int]
prelude_Char_Show_showsPrec prec i rest = str_ (showsPrec prec (chr i) []) ++ rest

prelude_Int_Show_showsPrec :: Int -> Int -> [Int] -> [Int]
prelude_Int_Show_showsPrec prec i rest = str_ (showsPrec prec i []) ++ rest

prelude_Integer_Show_showsPrec :: Int -> Integer -> [Int] -> [Int]
prelude_Integer_Show_showsPrec prec i rest = str_ (showsPrec prec i []) ++ rest

prelude_Double_Show_showsPrec :: Int -> Double -> [Int] -> [Int]
prelude_Double_Show_showsPrec prec i rest = str_ (showsPrec prec i []) ++ rest


overlay_arrayInt_array = array :: (Int,Int) -> [(Int,b)] -> Array Int b
overlay_arrayInt_index = (!) :: Array Int b -> Int -> b

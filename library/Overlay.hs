
-- override things in the libraries

module Overlay where

import System.IO(Handle, hGetContents, hGetChar)
import Data.Char(ord,chr)
import Foreign(unsafePerformIO)


---------------------------------------------------------------------
-- Prelude

-- make error primitive
foreign import primitive global_Prelude''error :: string -> bottom
global_Prelude''_patternMatchFail c = error c
global_Prelude''_noMethodError c = error c

foreign import primitive global_Prelude''seq :: a -> b -> b


---------------------------------------------------------------------
-- System.IO


foreign import primitive global_System'_IO''stdin  :: handle
foreign import primitive global_System'_IO''stdout :: handle
foreign import primitive global_System'_IO''stderr :: handle


foreign import primitive global_Data'_Char''isSpace :: char -> int


{-
-- IO Monad Version 1, custom design

data IO a = IO a

global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_return a = IO a
global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_'gt'gt (IO a) b = b
global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_'gt'gt'eq (IO a) f = f a
global_YHC'_Internal'_unsafePerformIO (IO a) = a


global_Prelude'_getContents = IO (unsafeContents token)

unsafeContents h =
    let x = get_char h
    in if x == (-1) then [] else x : unsafeContents h

global_System'_IO'_hGetChar h = 
    let x = get_char h
    in x `seq` IO x

global_System'_IO'_hPutChar h c =
    let x = put_char h c
    in x `seq` IO ()

foreign import primitive get_char :: token -> Int
foreign import primitive put_char :: handle -> Int -> Int

-}

-- IO Monad Version 2, GHC's design

type TIO a = State -> NIO a

data NIO a = NIO State a
data State = State

global_Prelude''YHC'_Internal'_IO''Prelude'_Monad''return a = returnIO a
global_Prelude''YHC'_Internal'_IO''Prelude'_Monad'''gt'gt a b = bindIO_ a b
global_Prelude''YHC'_Internal'_IO''Prelude'_Monad'''gt'gt'eq a b = bindIO a b
global_Prelude''YHC'_Internal'_IO''Prelude'_Monad''fail msg r = errorIO msg r
global_YHC'_Internal''unsafePerformIO a = unsafeIO a

foreign import primitive global_realWorld :: State
foreign import primitive global_typeRealWorld :: State -> State
foreign import primitive errorIO :: [String] -> TIO a


returnIO :: a -> TIO a
returnIO x s = NIO s x


bindIO :: TIO a -> (a -> TIO b) -> TIO b
bindIO m k s = case m (global_typeRealWorld s) of
    NIO news a -> k a news


bindIO_ :: TIO a -> TIO b -> TIO b
bindIO_ m k s = case m (global_typeRealWorld s) of
    NIO news a -> k news


unsafeIO :: TIO a -> a
unsafeIO m = case m global_realWorld of
    NIO news a -> a


interIO :: TIO a -> TIO a
interIO m s = let r = case m (global_typeRealWorld s) of NIO _ res -> res
              in NIO s r
-- returnIO (unsafeIO x)


global_Prelude''getContents =
    get_char `bindIO` \c ->
    if c == -1 then
        returnIO []
    else
        interIO global_Prelude''getContents `bindIO` \cs ->
        returnIO ((toEnum c :: Char) : cs)


-- if we make a primitive IO, it adds a mkIO wrapper, so avoid that
foreign import primitive get_char :: TIO Int

foreign import primitive global_Prelude''getChar :: TIO Char
--foreign import primitive global_Prelude''putChar :: Char -> TIO ()

foreign import primitive global_System'_IO''hGetChar :: Handle -> TIO Char
foreign import primitive global_System'_IO''hPutChar :: Handle -> Char -> TIO ()


foreign import primitive global_System'_Environment''getArgs :: TIO [String]


-- things which are just way too complex
foreign import primitive global_Prelude''Prelude'_Double''Prelude'_Show''showsPrec :: a -> Double -> ShowS
foreign import primitive global_Prelude''Prelude'_Int''Prelude'_Read''readsPrec :: Int -> ReadS Int
foreign import primitive global_Prelude''Prelude'_Int''Prelude'_Read''readList :: a

foreign import primitive global_Prelude''Prelude'_Integer''Prelude'_Read''readsPrec :: Int -> ReadS Integer
foreign import primitive global_Prelude''Prelude'_Integer''Prelude'_Read''readList :: a

foreign import primitive global_Prelude''Prelude'_Char''Prelude'_Read''readsPrec :: Int -> ReadS Char
foreign import primitive global_Prelude''Prelude'_Char''Prelude'_Read''readList :: a

foreign import primitive global_Prelude''Prelude'_Char''Prelude'_Show''showsPrec :: a
foreign import primitive global_Prelude''Prelude'_Char''Prelude'_Show''showList :: a

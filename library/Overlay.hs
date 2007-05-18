
-- override things in the libraries

module Overlay where

import System.IO(Handle, hGetContents, hGetChar)
import Data.Char(ord)
import Foreign(unsafePerformIO)


---------------------------------------------------------------------
-- Prelude

-- make error primitive
foreign import primitive global_Prelude'_error :: string -> bottom
global_Prelude'__patternMatchFail c = error c
global_Prelude'__noMethodError c = error c

foreign import primitive global_Prelude'_seq :: a -> b -> b


---------------------------------------------------------------------
-- System.IO


foreign import primitive global_System'_IO'_stdin  :: handle
foreign import primitive global_System'_IO'_stdout :: handle
foreign import primitive global_System'_IO'_stderr :: handle

foreign import primitive token :: a

foreign import primitive global_Data'_Char'_isSpace :: char -> int


-- IO stuff
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



-- override things in the libraries

module Overlay where

import System.IO(Handle, hGetContents, hGetChar)
import Data.Char(ord)
import Foreign(unsafePerformIO)


---------------------------------------------------------------------
-- Prelude

-- make error primitive
foreign import primitive global_Prelude'_error :: a -> b
global_Prelude'__patternMatchFail c = error c
global_Prelude'__noMethodError c = error c

foreign import primitive global_Prelude'_seq :: a -> b -> b


---------------------------------------------------------------------
-- System.IO


foreign import primitive global_System'_IO'_stdin  :: a
foreign import primitive global_System'_IO'_stdout :: a
foreign import primitive global_System'_IO'_stderr :: a
foreign import primitive global_System'_IO'_hGetChar :: a
foreign import primitive global_System'_IO'_hPutChar :: a


-- IO stuff
data IO a = IO a

global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_return a = IO a
global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_'gt'gt (IO a) b = b
global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_'gt'gt'eq (IO a) f = f a
global_YHC'_Internal'_unsafePerformIO (IO a) = a


global_System'_IO'_hGetContents h = do
    c <- hGetChar h
    if c == '\0'
        then return []
        else do
            let cs = unsafePerformIO (hGetContents h)
            return (c:cs)
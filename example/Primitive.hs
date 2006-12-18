
-- override things in the Prelude

module Primitive where

foreign import primitive global_Prelude'_error :: a -> b
foreign import primitive global_System'_IO'_stdout :: a
foreign import primitive global_System'_IO'_stdin :: a
foreign import primitive global_System'_IO'_stderr :: a
foreign import primitive global_System'_IO'_hPutChar :: a -> b -> c

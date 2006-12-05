
-- override things in the Prelude

module Primitive where

foreign import primitive global_Prelude'_error :: String -> a

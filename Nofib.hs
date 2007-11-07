
module Nofib(nofib) where


type Benchmark = String

--                                      errmsg executable
type Compiler = Benchmark -> IO (Either String String)


nofib :: String -> [(String,Compiler)] -> [Benchmark] -> IO ()
nofib root comps benchs = return ()


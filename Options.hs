
module Options where

data Options = Options {
                   optNofibLocation :: FilePath,
                   optObjLocation :: FilePath
               }


readOptions :: IO Options
readOptions = return $ Options "" ""

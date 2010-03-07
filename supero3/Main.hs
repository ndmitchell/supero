
module Main where


main = do
    xs <- getArgs
    forM_ xs $ \x -> do
        src <- parseFileContents xs
        

module Support(testEqual) where

import Util
import Control.Monad

testEqual :: String -> IO () -> IO () -> IO ()
testEqual name orig opt = do
    putStr $ "Testing " ++ name ++ "... "
    a <- captureOutput orig
    b <- captureOutput opt
    when (a /= b) $ do
        putStrLn "\nFATAL: Results do not match"
        putStrLn "WANTED:"
        putStrLn a
        putStrLn "GOT:"
        putStrLn b
    putStrLn "success"

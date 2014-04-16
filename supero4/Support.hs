
module Support(testEqual) where

testEqual :: String -> IO () -> IO () -> IO ()
testEqual name orig opt = do
    putStrLn $ "Testing " ++ name
    orig
    opt

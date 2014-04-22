{-# LANGUAGE PatternGuards #-}

module Util(module Util, module Safe, trace) where

import System.Info
import System.Cmd
import Control.Exception
import System.Exit
import Data.Function
import Data.List
import Control.Monad.State
import Data.IORef
import Debug.Trace
import System.IO.Unsafe
import Data.Time.Clock.POSIX(getPOSIXTime)
import System.Directory
import System.FilePath
import Data.Char
import System.IO
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
import Safe
import System.Environment


rlookup :: Eq a => a -> [(b,a)] -> Maybe b
rlookup x y = lookup x $ map swap y

swap (x,y) = (y,x)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)


subset x y = null $ x \\ y

fixEq f x = if x == x2 then x else fixEq f x2
    where x2 = f x


getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime

timer :: IO () -> IO ()
timer act = do
    start <- getTime
    act
    end <- getTime
    print (end - start)



delFst :: Eq a => a -> [(a,b)] -> [(a,b)]
delFst x = filter ((/=) x . fst)

delFsts :: Eq a => [a] -> [(a,b)] -> [(a,b)]
delFsts x = filter (flip notElem x . fst)



freshVars :: String -> [String]
freshVars v = [v ++ show i | i <- [1..]]


class FreshState a where
    getFresh :: a -> [String]
    setFresh :: a -> [String] -> a


fresh :: FreshState a => State a String
fresh = do
    s <- get
    let v:vs = getFresh s
    put $ setFresh s vs
    return v


freshN :: FreshState a => Int -> State a [String]
freshN n = do
    s <- get
    let (v,vs) = splitAt n $ getFresh s
    put $ setFresh s vs
    return v


filterFresh :: FreshState a => (String -> Bool) -> State a ()
filterFresh f = modify $ \s -> setFresh s $ filter f $ getFresh s


type Fresh a = State SFresh a
newtype SFresh = SFresh [String]

instance FreshState SFresh where
    getFresh (SFresh x) = x
    setFresh _ x = SFresh x

runFresh :: String -> Fresh a -> a
runFresh v x = evalState x $ SFresh $ freshVars v



{-# NOINLINE time #-}
time :: Int -> Bool
time i = unsafePerformIO $ do
    n <- readIORef timeRef
    writeIORef timeRef (n+1)
    return $ i == n

{-# NOINLINE timeRef #-}
timeRef :: IORef Int
timeRef = unsafePerformIO $ newIORef 0


{-# NOINLINE resetTime #-}
resetTime :: a -> a
resetTime x = unsafePerformIO $ do
    writeIORef timeRef 0
    return x


type Id x = x -> x

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
    xs <- getDirectoryContents dir
    (dirs,files) <- partitionM doesDirectoryExist [dir </> x | x <- xs, not $ isBadDir x]
    rest <- concatMapM getDirectoryContentsRecursive $ sort dirs
    return $ sort files ++ rest
    where
        isBadDir x = "." `isPrefixOf` x || "_" `isPrefixOf` x


partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

lower = map toLower
upper = map toUpper

captureOutput :: IO () -> IO String
captureOutput act = do
    tmp <- getTemporaryDirectory
    (f,h) <- openTempFile tmp "hlint"
    sto <- hDuplicate stdout
    ste <- hDuplicate stderr
    hDuplicateTo h stdout
    hDuplicateTo h stderr
    hClose h
    act
    hDuplicateTo sto stdout
    hDuplicateTo ste stderr
    res <- readFile' f
    removeFile f
    return res

captureStdout :: IO () -> IO String
captureStdout act = do
    tmp <- getTemporaryDirectory
    (f,h) <- openTempFile tmp "hlint"
    sto <- hDuplicate stdout
    hDuplicateTo h stdout
    hClose h
    act
    hDuplicateTo sto stdout
    res <- readFile' f
    removeFile f
    return res

readFile' :: FilePath -> IO String
readFile' x = listM' =<< readFile x

listM' :: Monad m => [a] -> m [a]
listM' x = length x `seq` return x

devNull = if os == "mingw32" then "nul" else "/dev/null"

withDirectory new act = do
    old <- getCurrentDirectory
    bracket_
        (setCurrentDirectory new)
        (setCurrentDirectory old)
        act

system_ cmd = do
    putStrLn cmd
    res <- system cmd
    when (res /= ExitSuccess) $ error $ "system command failed: " ++ cmd

fast = "--fast" `elem` unsafePerformIO getArgs

idempotent :: (ShowNice a, Eq a) => String -> (a -> a) -> (a -> a)
idempotent name f x0
    | fast = x1
    | x1 == x2 = x1
    | otherwise = error $ unlines
        ["START Idempotent check failed for " ++ name ++ "!"
        ,"Input:"
        ,showNice x0
        ,"After first application:"
        ,showNice x1
        ,"After second application:"
        ,showNice x2
        ,"END Idempotent check failed for " ++ name ++ "!"
        ]
    where x1 = f x0
          x2 = f x1

equivalentOn :: (ShowNice a, ShowNice b, Eq b) => (a -> b) -> String -> a -> a -> a
equivalentOn op name x y
    | fast = y
    | xx == yy = y
    | otherwise = unsafePerformIO $ do
        writeFile "error.log" $ "-- Equivalent check failed for " ++ name ++ "\n" ++ showNice x
        error $ unlines
            ["START Equivalent check failed for " ++ name ++ "!"
            ,"Input:"
            ,showNice x
            ,"Output:"
            ,showNice y
            ,"Input (reduced):"
            ,showNice xx
            ,"Output (reduced):"
            ,showNice yy
            ,"END Equivalent check failed for " ++ name ++ "!"
            ]
    where xx = op x
          yy = op y

class ShowNice a where showNice :: a -> String

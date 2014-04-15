{-# LANGUAGE PatternGuards #-}

module Util(module Util, trace) where

import Data.Function
import Data.List
import Control.Monad.State
import Data.IORef
import Debug.Trace
import System.IO.Unsafe
import Data.Time.Clock.POSIX(getPOSIXTime)


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


fromJustNote msg Nothing = error $ "fromJustNote: " ++ msg
fromJustNote msg (Just x) = x


type Id x = x -> x

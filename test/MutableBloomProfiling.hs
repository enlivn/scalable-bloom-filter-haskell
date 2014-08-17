{-|
Module      : Main
Description : Profiling performance of Mutable bloom filter
-}

{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad (liftM)
import Control.Parallel.Strategies (NFData, rdeepseq, withStrategy)
import Control.Monad.ST (ST)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.ByteString.Char8 as L (readFile, lines)
import qualified ImmutableBloomFilter as Im (elem, ImmutableBloom)
import qualified MutableBloomFilter as M (toImmutable, fromList)
import System.Environment (getArgs)

instance NFData (ST s a)
instance NFData (Im.ImmutableBloom a)

-- | Measure time taken to perform the given action
-- The first argument is an informative message to print
-- The second argument is the action to perform
timeIt :: NFData a => String -> IO a -> IO a
timeIt msg action = do
    start <- getCurrentTime
    result <- (return . withStrategy rdeepseq) =<< action
    end <- getCurrentTime
    putStrLn $ msg ++ " took " ++ show (diffUTCTime end start)
    return result

main :: IO ()
main = do
    fileList <- getArgs
    if null fileList then do
        let fList = ["/usr/share/dict/words"]
        mapM processFile fList
    else
        mapM processFile fileList
    return ()
    where processFile :: String -> IO ()
          processFile fileName = do
            contents <- timeIt "processing file" $ liftM L.lines (L.readFile fileName)
            mb <- timeIt "constructing filter" $ return $ M.toImmutable $ M.fromList 0.01 contents
            results <- timeIt "querying filter" $ return $ map (Im.elem mb) contents
            if (not . and) results then do
                error "Not all elements inserted were found"
                return ()
            else
                return ()

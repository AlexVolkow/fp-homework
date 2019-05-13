module HashTableBench
    ( hashTableStressTable
    ) where

import Criterion.Main (bench, nfIO, Benchmark)
import Control.Concurrent.Async (async, wait)
import HashTable (ConcurrentHashTable, newCHT, putCHT, getCHT)
import System.Random (randomIO, randomRIO)

hashTableStressTable :: [Benchmark]
hashTableStressTable = map (\(putPtr, threads) ->
    bench ("threads " ++ show (threads) ++ ", put " ++ (show (round (putPtr * 100) :: Int)) ++ ", get " ++ (show (round ((1 - putPtr) * 100) :: Int))) $
     nfIO (hashTableTest putPtr threads)) ([0.8, 0.2] `vectorProduct` [1, 4, 8, 16])

hashTableTest :: Double -> Int -> IO ()
hashTableTest putPtr threads = do
    tasks <- mapM (\_ -> async $ runTest putPtr (100000 `div` threads)) [0..threads]
    _ <- mapM wait tasks
    return ()

runTest :: Double -> Int -> IO ()
runTest putPtr iteration = do
    table <- newCHT
    _ <- mapM (\_ -> doTest table putPtr) [0..iteration]
    return ()

doTest :: ConcurrentHashTable Int Int -> Double -> IO ()
doTest myht putPtr = do
    action <- randomIO
    if (action < putPtr)
    then do
        v <- randomRIO (0, 1000::Int)
        k <- randomRIO (0, 500::Int)
        _ <- putCHT k v myht
        return ()
    else do
        k <- randomRIO (0, 500::Int)
        _ <- getCHT k myht
        return ()

vectorProduct :: [a] -> [b] -> [(a, b)]
vectorProduct a b = concatMap (\a1 -> map (\b1 -> (a1, b1)) b) a
module Lib
       ( order3
       , smartReplicate
       , contains
       , stringSum
       , removeAt
       , mergeSort
       ) where

import           Data.List     (sort, splitAt)
import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

--Block 1

--Task 1
order3 :: Ord v => (v, v, v) -> (v, v, v)
order3 (v1, v2, v3) = (u1, u2, u3)
    where [u1, u2, u3] = sort [v1, v2, v3]

--Task 2
smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

--Task 3
contains :: Eq a => a -> [[a]] -> [[a]]
contains val list = filter (elem val) list

--Task 4
stringSum :: String -> Int
stringSum str = sum $ map read $ words str

--Block 2

--Task 1
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt idx list = case splitAt idx list of
    (xs, [])   -> (Nothing, xs)
    ([], ys)   -> (Nothing, ys)
    (xs, y:ys) -> (Just y, xs ++ ys)

--Task 2
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = let mid = (length list) `div` 2 in
                    merge (mergeSort $ take mid list) (mergeSort $ drop mid list)
  where
    merge :: (Ord a) => [a] -> [a] -> [a]
    merge x [] = x
    merge [] y = y
    merge (x:xs) (y:ys)
      | x < y     = x:(merge xs (y:ys))
      | otherwise = y:(merge (x:xs) ys)

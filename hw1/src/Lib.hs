module Lib
       ( order3
       , smartReplicate
       , contains
       , stringSum
       ) where

import           Data.List (sort)

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
{-# LANGUAGE Strict       #-}

module Matrix
    ( multiply
    ) where

import qualified Control.Foldl as F
import Control.Parallel.Strategies
import Data.List (transpose)

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply x y = if (not(length (head x) == length y)) then Nothing else Just $ runMultiply x (transpose y)
    where
        runMultiply :: [[Int]] -> [[Int]] -> [[Int]]
        runMultiply a b = parMap rpar (\row -> multiplyHelper row b) a

        multiplyHelper :: [Int] -> [[Int]] -> [Int]
        multiplyHelper row m = runEval (rdeepseq $ map (multiplyRow row) m)

        multiplyRow :: [Int] -> [Int] -> Int
        multiplyRow r c = F.fold F.sum (zipWith (*) r c)


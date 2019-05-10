module MatrixUtils
    ( grid
    , multiplySimple
    ) where

import Control.Monad (replicateM)
import Data.List (transpose)
import System.Random (randomRIO)

randomList :: Int -> IO ([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1, 10000)
  rs <- randomList (n-1)
  return (r:rs)

grid :: Int -> Int -> IO ([[Int]])
grid x y = replicateM x (randomList y)

multiplySimple :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplySimple x y = runMultiply x (transpose y)
    where
        runMultiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
        runMultiply a b = Just $ fmap (\row -> multiplyHelper row b) a

        multiplyHelper :: [Int] -> [[Int]] -> [Int]
        multiplyHelper row m = map (multiplyRow row) m

        multiplyRow :: [Int] -> [Int] -> Int
        multiplyRow [] []         = 0
        multiplyRow (r:rs) (c:cs) = (r * c) + multiplyRow rs cs
        multiplyRow _ _           = error "incorrect dimensions of matrix"


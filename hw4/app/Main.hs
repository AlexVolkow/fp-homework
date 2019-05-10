module Main where

import Matrix (multiply)
import MatrixUtils (grid)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    line1 <- getLine
    line2 <- getLine
    let n = read line1 :: Int
    let m = read line2 :: Int
    matrix1 <- grid n m
    matrix2 <- grid m n
    let result = multiply matrix1 matrix2
    putStrLn (show matrix1)
    putStrLn (show matrix2)
    putStrLn $ show $ fromJust result

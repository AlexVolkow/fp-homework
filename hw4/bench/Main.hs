module Main where

import Criterion.Main
import MatrixBench (matrixMultParallel, matrixMultSequential)
import GeometryBench (fastPerimeter, fastArea, slowPerimeter, slowArea)
import GaussBench (guassSimple, guassOnVector)
import HashTableBench (hashTableStressTable)

main :: IO ()
main = do
  mbench1 <- matrixMultParallel
  mbench2 <- matrixMultSequential

  defaultMain [
    bgroup "matrix multiplication parallel" mbench1,
    bgroup "matrix multiplication sequential" mbench2,
    bgroup "geometry perimeter fast" fastPerimeter,
    bgroup "geometry perimeter slow" slowPerimeter,
    bgroup "geometry doubleArea fast" fastArea,
    bgroup "geometry doubleArea slow" slowArea,
    bgroup "gauss bench on vector" guassOnVector,
    bgroup "gauss bench simple" guassSimple
    bgroup "hash table stress" hashTableStressTable
    ]
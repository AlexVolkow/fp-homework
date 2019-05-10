module Main where

import Criterion.Main
import MatrixBench (matrixMultParallel, matrixMultSequential)
import GeometryBench (fastPerimeter, fastArea, slowPerimeter, slowArea)

main :: IO ()
main = do
  mbench1 <- matrixMultParallel
  mbench2 <- matrixMultSequential

  defaultMain [
    bgroup "geometry perimeter fast" fastPerimeter,
    bgroup "geometry perimeter slow" slowPerimeter,
    bgroup "geometry doubleArea fast" fastArea,
    bgroup "geometry doubleArea slow" slowArea,
    bgroup "matrix multiplication parallel" mbench1,
    bgroup "matrix multiplication sequential" mbench2
    ]
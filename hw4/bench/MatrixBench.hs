module MatrixBench
 ( matrixMultSequential
 , matrixMultParallel
 ) where

import Criterion.Main (bench, nf, Benchmark)
import Matrix (multiply)
import MatrixUtils (multiplySimple, grid)

grids :: IO [(Int, [[Int]], [[Int]])]
grids = sequence $ map generate [200, 400, 800]
    where
        generate x = do
            m1 <- grid x x
            m2 <- grid x x
            return (x, m1, m2)

matrixMultSequential :: IO [Benchmark]
matrixMultSequential = fmap (\x -> fmap (\(a, l, r) -> bench ("sequential " ++ (show a)) $ nf (multiplySimple l) r) x) grids

matrixMultParallel :: IO [Benchmark]
matrixMultParallel = fmap (\x -> fmap (\(a, l, r) -> bench ("parallel " ++ (show a)) $ nf (multiply l) r) x) grids

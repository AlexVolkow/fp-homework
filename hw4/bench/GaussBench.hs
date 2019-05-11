module GaussBench
 ( guassSimple
 , guassOnVector
 ) where

import Criterion.Main (bench, nf, Benchmark)
import GaussUtils (gaussSlow)
import Gauss (gauss)

grids :: [(Int, [[Bool]], [Bool])]
grids = map go [1000, 5000]
    where
        go x = (x, reverse $ triangle x, replicate x True)

guassSimple :: [Benchmark]
guassSimple = fmap (\(a, l, r) -> bench ("gauss simple " ++ (show a)) $ nf (gaussSlow l) r) grids

guassOnVector :: [Benchmark]
guassOnVector = fmap (\(a, l, r) -> bench ("gauss on vector " ++ (show a)) $ nf (gauss l) r) grids

nTimes :: Int -> (Int -> a) -> [a]
nTimes 0 _ = []
nTimes n f = nTimes (n - 1) f ++ [f n]

triangle :: Int -> [[Bool]]
triangle 0 = []
triangle x = nTimes x (\i -> ((replicate (i - 1) False) ++ (replicate (x - i + 1) True)))
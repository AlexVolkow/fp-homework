module GeometryBench where

import Criterion.Main (bench, nf, Benchmark)
import Geometry (perimeter, doubleArea, Point(..))
import GeometryUtils (perimeterSlow, doubleAreaSlow)

polygons :: [[Point]]
polygons = map generate [10000000, 50000000]
    where
        generate l = map (\x -> Point x x) [0..l]

fastPerimeter :: [Benchmark]
fastPerimeter = map (\polygon -> bench ("fast perimeter " ++ (show $ length polygon)) $ nf perimeter polygon) polygons

fastArea :: [Benchmark]
fastArea = map (\polygon -> bench ("fast area " ++ (show $ length polygon)) $ nf doubleArea polygon) polygons

slowPerimeter :: [Benchmark]
slowPerimeter = map (\polygon -> bench ("slow perimeter " ++ (show $ length polygon)) $ nf perimeterSlow polygon) polygons

slowArea :: [Benchmark]
slowArea = map (\polygon -> bench ("slow area " ++ (show $ length polygon)) $ nf doubleAreaSlow polygon) polygons
module GeometryUtils
 ( perimeterSlow,
   doubleAreaSlow
 ) where

import Geometry (Point(..), minus, len)

perimeterSlow :: [Point] -> Double
perimeterSlow [] = 0
perimeterSlow a@(x : xs) = foldl (+) 0 (map (\(p1, p2) -> len $ p2 `minus` p1) (zip a (xs ++ [x])))

doubleAreaSlow :: [Point] -> Int
doubleAreaSlow [] = 0
doubleAreaSlow a@(x : xs) = foldl (+) 0 (map (\(p1, p2) -> square p1 p2) $ zip a (xs ++ [x]))
     where
        square (Point x1 y1) (Point x2 y2) = (x1 - x2) * (y1 + y2)
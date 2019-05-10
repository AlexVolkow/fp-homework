{-# LANGUAGE Strict       #-}

module Geometry
   ( Point (..)
   , plus
   , minus
   , scalarProduct
   , crossProduct
   , perimeter
   , doubleArea
   , len
   ) where

import Data.List (foldl')

data Point = Point Int Int deriving (Show, Eq)

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = (+) (x1 * x2) $ (y1 * y2)

crossProduct  :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = (-) (x1 * y2) $ (y1 * x2)

len :: Point -> Double
len (Point x y) = sqrt $ fromIntegral (x * x + y * y)

perimeter :: [Point] -> Double
perimeter = calcPolygon (\p1 p2 -> len $ p2 `minus` p1) 0

doubleArea :: [Point] -> Int
doubleArea = calcPolygon square 0
    where
        square (Point x1 y1) (Point x2 y2) = (x1 - x2) * (y1 + y2)

calcPolygon :: Num a => (Point -> Point -> a) -> a -> [Point] -> a
calcPolygon f z = calc
    where
        calc [] = z
        calc (x: xs) = let (lastPoint, ans) = foldl' (\(prev, acc) curr -> (curr, acc + f prev curr)) (x, z) xs in
            ans + f lastPoint x


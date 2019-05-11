{-# LANGUAGE Strict #-}

module Gauss
 ( gauss
 , verifySolution
 , triangular
 , resubstitute
 , toVector
 , rotatePivot
 , xorLines
 ) where

import qualified Data.Vector as V
import Control.Parallel.Strategies (rpar, runEval, evalTraversable, parMap)
import Data.List (foldl')

type Row = V.Vector Bool
type Matrix = V.Vector Row

gauss :: [[Bool]] -> [Bool] -> Maybe [Bool]
gauss m b = if (isSquare m) then fmap V.toList (gaussVector (toVector m) (V.fromList b)) else Nothing

gaussVector :: Matrix -> Row -> Maybe Row
gaussVector a b = x
     where
        b' = fmap V.singleton b
        a' = V.zipWith (V.++) a b'
        x  = fmap resubstitute (triangular a')

triangular :: Matrix -> Maybe Matrix
triangular m
 | V.null m = pure V.empty
 | otherwise = case (rotatePivot m) of
    Just rotated -> do
        let row = V.unsafeHead rotated
        let rows = V.unsafeTail rotated
        let rows' = parMapVec (xorLines row) rows
        (V.cons) <$> pure row <*> (triangular rows')

    Nothing -> Nothing

xorLines :: Row -> Row -> Row
xorLines row bs
                | (V.unsafeHead bs) == False = V.drop 1 bs
                | otherwise = V.drop 1 $ V.zipWith (xor) bs row

rotatePivot :: Matrix -> Maybe Matrix
rotatePivot m = case (firstNotFalse m) of
    Just 0 -> pure m
    Just idx -> pure $ m `V.unsafeUpd` [(0, (V.unsafeIndex m idx)), (idx, row)]
    Nothing -> Nothing
    where
        row = V.unsafeHead m
        firstNotFalse = V.findIndex (\r -> (V.unsafeHead r) /= False)


resubstitute :: Matrix -> Row
resubstitute m = foldl' (\v' -> go m v') V.empty [matrixSize, (matrixSize - 1)..0]
    where
      matrixSize = V.length m - 1

      go :: Matrix -> Row -> Int -> Row
      go matrix variables rowIdx = do
        let lastRow = V.unsafeIndex matrix rowIdx
        let newVar = resubstitute' lastRow variables
        newVar `V.cons` variables

resubstitute' :: Row -> Row -> Bool
resubstitute' row variables = ret
    where
        knownPart = V.foldl' xor False (V.zipWith (&&) (dropBoth 1 row) variables)
        ret = knownPart `xor` (V.unsafeLast row)

dropLast :: Int -> Row -> Row
dropLast n v = V.take (V.length v - n) v

dropBoth :: Int -> Row -> Row
dropBoth n v = V.drop n (dropLast n v)

xor :: Bool -> Bool -> Bool
xor = (/=)

verifySolution :: [[Bool]] -> [Bool] -> [Bool] -> Bool
verifySolution a b c = foldl' (&&) True (parMap rpar check (zip a b))
    where
        check (r, ans) = ans == r `multiplyRows` c

multiplyRows :: [Bool] -> [Bool] -> Bool
multiplyRows a b = foldl xor False (Prelude.zipWith (&&) a b)

parMapVec :: (a -> b) -> V.Vector a -> V.Vector b
parMapVec f v = runEval $ evalTraversable rpar $ V.map f v

toVector :: [[Bool]] -> Matrix
toVector x = V.fromList $ map V.fromList x

isSquare :: [[Bool]] -> Bool
isSquare m = length m == (length $ head m)
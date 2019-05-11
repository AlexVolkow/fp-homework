module GaussUtils
    ( gaussSlow
    , verifySolutionSlow
    ) where

gaussSlow :: [[Bool]] -> [Bool] -> Maybe [Bool]
gaussSlow a b = x
     where
        b' = map (\y -> [y]) b
        a' = zipWith (++) a b'
        x  = fmap resubstitute (triangular a')

triangular :: [[Bool]] -> Maybe [[Bool]]
triangular [] = pure []
triangular m  = case (rotatePivot m) of
 Just (row: rows) -> do
     let rows' = map (xorLines row) rows
     (:) <$> pure row <*> (triangular rows')

 Just [] -> Nothing
 Nothing -> Nothing

xorLines :: [Bool] -> [Bool] -> [Bool]
xorLines row bs
                | (head bs) == False = drop 1 bs
                | otherwise = drop 1 $ zipWith (xor) bs row

rotatePivot :: [[Bool]] -> Maybe [[Bool]]
rotatePivot [] = Nothing
rotatePivot (row:rows)
     | (head row) /= False = pure (row:rows)
     | otherwise       = do
        rows' <- rotatePivot rows
        pure (rows' ++ [row])

resubstitute :: [[Bool]] -> [Bool]
resubstitute = reverse . resubstitute' . reverse . map reverse

resubstitute' :: [[Bool]] -> [Bool]
resubstitute' [] = []
resubstitute' (row:rows) = x:(resubstitute' rows')
     where
     x     = (head row) && (last row)
     rows' = map substituteUnknown rows
     substituteUnknown (a1:(a2:as')) = ((a1 `xor` (x && a2)):as')
     substituteUnknown _ = error "invalid demensions of matrix"

xor :: Bool -> Bool -> Bool
xor = (/=)

mapMatrix :: [[Bool]] -> [Bool] -> [Bool]
mapMatrix rows v = [foldl xor False (zipWith (&&) row v) | row <- rows]

verifySolutionSlow :: [[Bool]] -> [Bool] -> [Bool] -> Bool
verifySolutionSlow a b c = mapMatrix a c == b
module GaussUtils
    ( gaussSlow
    ) where

gaussSlow :: [[Bool]] -> [Bool] -> Maybe [Bool]
gaussSlow m1 m2 = fmap resubstitute (triangular (zipWith (++) m1 (map (\y -> [y]) m2)))

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
resubstitute = reverse . go . reverse . map reverse
    where
    go [] = []
    go (row : rows) = x : (go (map dosubstitute rows))
         where
         x = (head row) && (last row)
         dosubstitute (a1 : (a2 : as')) = ((a1 `xor` (x && a2)):as')
         dosubstitute _ = error "invalid demensions of matrix"

xor :: Bool -> Bool -> Bool
xor = (/=)
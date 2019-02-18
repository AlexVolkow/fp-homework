{-# LANGUAGE InstanceSigs #-}

module Foldable
  ( Pair (..)
  , NonEmpty (..)
  , splitOn
  ) where

--Block 4

--Task 1

data Pair a = Pair a a

instance Foldable Pair where
  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair x y) = f x <> f y

  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair x y) = f x $ f y z

data NonEmpty a = a :| [a]

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = f x <> foldMap f xs

  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = f x $ foldr f z xs

--Task 2

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = foldr f [[]]
  where
    f item (y:ys)
      | item == x = [] : y : ys
      | otherwise = (item : y) : ys
    f _ [] = undefined
{-# LANGUAGE InstanceSigs #-}

module Block1
    ( stringSum,
      Tree,
      NonEmpty,
      fromList
    ) where

import Data.Foldable (toList)
import Text.Read (readMaybe)

--Task 1
stringSum :: String -> Maybe Int
stringSum s = sum <$> traverse (readMaybe) (words s)

--Task 2

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving Show

instance Functor Tree where
    fmap f (Leaf x)     = Leaf $ f x
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
    pure = Leaf

    (<*>) (Leaf f) t     = fmap f t
    (<*>) (Branch l r) t = Branch (l <*> t) (r <*> t)

instance Foldable Tree where
    foldr f acc (Leaf x)     = f x acc
    foldr f acc (Branch l r) = foldr f (foldr f acc r) l

instance Traversable Tree where
   traverse f (Leaf x)     = Leaf <$> f x
   traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r

--Task 3

data NonEmpty a = a :| [a] deriving Show

fromList :: [a] -> NonEmpty a
fromList (x:xs) = x :| xs
fromList _      = undefined

instance Functor NonEmpty where
    fmap f (x :| xs) = f x :| map f xs

instance Applicative NonEmpty where
    pure x = x :| []
    (<*>) (f:|fs) (x:|xs) = fromList [g y | g <- f:fs, y <- x:xs]

instance Monad NonEmpty where
    return = pure
    (>>=) (l:|ls) f = x :| (xs ++ ys)
                  where
                    x :| xs = f l
                    ys = ls >>= toList . f

instance Foldable NonEmpty where
    foldMap f (x :| xs) = f x `mappend` foldMap f xs

instance Traversable NonEmpty where
    traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs


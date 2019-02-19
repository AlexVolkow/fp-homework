module Monoids
  ( maybeConcat
  , ThisOrThat
  , Builder
  , fromString
  , toString
  ) where

import           Data.Semigroup (Semigroup (..))

--Block 5

--Task 1
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr f []
  where
    f Nothing acc     = acc
    f (Just list) acc = list ++ acc

--Task 2
data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty t) where
  (x :| xs) <> (y :| ys) = x :| (xs ++ [y] ++ ys)

data ThisOrThat a b = This a | That b | Both a b

instance Semigroup (ThisOrThat a b) where
    (This a) <> (That b) = Both a b
    (That a) <> (This b) = Both b a
    c <> _ = c

--Task 3

data Builder = One Char | Many [Builder]

instance Semigroup Builder where
    x@(One _) <> y@(One _) = Many [x, y]
    (Many xs) <> (Many ys) = Many (xs ++ ys)
    b <> (Many bs) = Many (b : bs)
    (Many bs) <> b = Many (bs ++ [b])

instance Monoid Builder
  where
    mempty  = Many []
    mappend = (<>)

fromString :: String -> Builder
fromString str = foldr (mappend . One) mempty str

toString :: Builder -> String
toString (One b)   = [b]
toString (Many bs) = foldr (mappend . toString) mempty bs

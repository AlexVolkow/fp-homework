{-# LANGUAGE TypeOperators #-}

module Lib
    ( distributivity
    , associator
    ) where

import           Data.Either
import           Data.Function (fix)
import           Data.Void     (Void)

--Task 1

distributivity
    :: Either a (b, c)
    -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator
    :: (a, (b, c))
    -> ((a, b), c)
associator (x,(y, z)) = ((x, y), z)

type (<->) a b = (a -> b, b -> a)

eitherAssoc
    :: Either a (Either b c)
    <-> Either (Either a b) c
eitherAssoc = (left, right)

left
    :: Either a (Either b c)
    -> Either (Either a b) c
left (Left a)          = Left (Left a)
left (Right (Left b))  = Left (Right b)
left (Right (Right c)) = Right c

right
    :: Either (Either a b) c
    -> Either a (Either b c)
right (Left (Left a))  = Left a
right (Left (Right b)) = Right (Left b)
right (Right c)        = Right (Right c)

--Task 2

{-type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg a = undefined

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = undefined

pierce :: ((a -> b) -> a) -> a
pierce = undefined

doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = undefined-}

--Task 3

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

k :: a -> b -> a
k = const

composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (k s) k

identity :: a -> a
identity a = a

contraction :: (a -> a -> b) -> a -> b
contraction = s s (k identity)

permutation :: (a -> b -> c) -> b -> a -> c
permutation =  (s (s (k (s (k s) k)) s) (k k))

--Task 4

iterateElement :: a -> [a]
iterateElement = fix go
      where
        go :: (a -> [a]) -> a -> [a]
        go f a = a : f a

fibonacci :: Integer -> Integer
fibonacci = fix go 1 0
      where
        go f a b 0 = b
        go f a b 1 = a
        go f a b n = f (a + b) a (n - 1)

factorial :: Integer -> Integer
factorial = fix go
      where
        go f 0 = 1
        go f n = n * f (n - 1)

mapFix :: (a -> b) -> [a] -> [b]
mapFix f = fix go
      where
        go _ []           = []
        go recurse (x:xs) =  f x : recurse xs

--Task 5

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero f x = x

succChurch :: Nat a -> Nat a
succChurch num = \f -> \x -> num f (f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m = \f -> \x -> ((n f) (m f x))

churchMult :: Nat a -> Nat a -> Nat a
churchMult n m = \f -> \x ->  n (m f) x

churchToInt :: Nat Integer  -> Integer
churchToInt num = num (+ 1) 0

--Task 6

--Task 7

a7 = null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
{-
a) . : (b -> c) -> (a -> b) -> (a -> c)
b) id : a -> a
c) uncurry : (a -> b -> c) -> (a, b) -> c
d) map : (a -> b) -> [a] -> [b]
e) null : t -> a -> Bool
f) $ : (a -> b) -> a -> b
g) head : [a] -> a
h) ++ : [a] -> [a] -> [a]
i) uncurry id : (b -> c, b) -> c
j) b, c : String
k) map (uncurry id) [] : ((b -> c, b) -> c) -> [(b -> c, b)] -> [c]
l) map (uncurry id) [((++) "Dorian ", " Grey")] = ["Dorian Grey"] : [String]
m) head $ map (uncurry id) [((++) "Dorian ", " Grey")] = "Dorian Grey" : String
n) null . head $ map (uncurry id) [((++) "Dorian ", " Grey")] = "False" : Bool
-}

b7 = (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
{-
a) lefts :: [Either a b] -> [a]
b) rights :: [Either a b] -> [b]
c) zip : [a] -> [b] -> [(a,b)]
d) Left (1 + 2) : Either Integer b
e) Right (2 ^ 6) : Either a Integer
f) [Left (1 + 2), Right (2 ^ 6)] : [(Either a b, Either a b)]
g) (\x -> zip (lefts x) (rights x)) : [Either a b] -> [(a, b)]
h)  (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]: [Either 3 64]
-}

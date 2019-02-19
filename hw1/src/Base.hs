{-# LANGUAGE InstanceSigs #-}

module Base
       ( order3
       , smartReplicate
       , contains
       , stringSum
       , removeAt
       , mergeSort
       , WeekDay(..)
       , nextDay
       , afterDays
       , isWeekend
       , daysToParty
       , Lord
       , Entertainment (..)
       , Citizens (..)
       , House (..)
       , Castle (..)
       , Walls (..)
       , Defence (..)
       , City (..)
       , buildCastle
       , buildEntertainment
       , buildHouse
       , newLord
       , buildWalls
       , Tree(..)
       , isEmpty
       , size
       , find
       , insert
       , fromList
       , removeNode
       ) where

import           Data.Foldable      (foldl')
import           Data.List          (sort, splitAt)
import           Data.List.NonEmpty (NonEmpty (..), cons)

--Block 1

--Task 1
order3 :: Ord v => (v, v, v) -> (v, v, v)
order3 (v1, v2, v3) = (u1, u2, u3)
    where [u1, u2, u3] = sort [v1, v2, v3]

--Task 2
smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

--Task 3
contains :: Eq a => a -> [[a]] -> [[a]]
contains val list = filter (elem val) list

--Task 4
stringSum :: String -> Int
stringSum str = sum $ map read $ words str

--Block 2

--Task 1
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt idx list = case splitAt idx list of
    (xs, [])   -> (Nothing, xs)
    ([], ys)   -> (Nothing, ys)
    (xs, y:ys) -> (Just y, xs ++ ys)

--Task 2
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = let mid = (length list) `div` 2 in
                    merge (mergeSort $ take mid list) (mergeSort $ drop mid list)
  where
    merge :: (Ord a) => [a] -> [a] -> [a]
    merge x [] = x
    merge [] y = y
    merge (x:xs) (y:ys)
      | x < y     = x:(merge xs (y:ys))
      | otherwise = y:(merge (x:xs) ys)

--Block 3

--Task 1
data WeekDay =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

instance Enum WeekDay where
    toEnum 0 = Monday
    toEnum 1 = Tuesday
    toEnum 2 = Wednesday
    toEnum 3 = Thursday
    toEnum 4 = Friday
    toEnum 5 = Saturday
    toEnum 6 = Sunday
    toEnum _ = error "WeekDay must be from 0 to 6"

    fromEnum Monday    = 0
    fromEnum Tuesday   = 1
    fromEnum Wednesday = 2
    fromEnum Thursday  = 3
    fromEnum Friday    = 4
    fromEnum Saturday  = 5
    fromEnum Sunday    = 6

instance Eq WeekDay where
    (==) :: WeekDay -> WeekDay -> Bool
    t1 == t2          = fromEnum t1 == fromEnum t2

nextDay :: WeekDay -> WeekDay
nextDay Sunday = Monday
nextDay day    = succ day

afterDays :: WeekDay -> Int -> WeekDay
afterDays day n = nextDay $ afterDays day (n - 1)

isWeekend :: WeekDay -> Bool
isWeekend day = day == Sunday || day == Saturday

daysToParty :: WeekDay -> Int
daysToParty = countDays 0
  where
    countDays :: Int -> WeekDay -> Int
    countDays acc Friday = acc
    countDays acc day    = countDays (acc + 1) (nextDay day)

--Task 2

data City = City
  { mDefence       :: Maybe Defence
  , mEntertainment :: Maybe Entertainment
  , mHouses        :: NonEmpty House
  }

data Entertainment = Library | Church

newtype House = House Citizens

data Citizens =
    One
  | Two
  | Three
  | Four

instance Enum Citizens where
    fromEnum One   = 1
    fromEnum Two   = 2
    fromEnum Three = 3
    fromEnum Four  = 4

    toEnum 1 = One
    toEnum 2 = Two
    toEnum 3 = Three
    toEnum 4 = Four
    toEnum _ = error "Unsupported count of citizens"

data Defence = Defence
  { mCastle :: Castle
  , mWalls  :: Maybe Walls
  }

type Lord = String

newtype Castle = Castle (Maybe Lord)

data Walls = Walls

buildCastle :: City -> Castle -> Either String City
buildCastle (City Nothing entertainment houses) castle =
  Right (City (Just (Defence castle Nothing)) entertainment houses)
buildCastle _ _ = Left "Castle already built"

buildEntertainment :: City -> Entertainment -> Either String City
buildEntertainment (City def Nothing houses) entr = Right (City def (Just entr) houses)
buildEntertainment _ _ = Left "Church or Library already built"

buildHouse :: City -> Citizens -> City
buildHouse (City def entr houses) family = City def entr (cons (House family) houses)

data LordError = NoCastle | LordExists

newLord :: City -> Lord -> Either LordError City
newLord (City Nothing _ _) _ = Left NoCastle
newLord (City (Just (Defence (Castle (Just _)) _)) _ _) _ = Left LordExists
newLord (City (Just (Defence (Castle Nothing) walls)) entr houses) lord
  = Right (City (Just (Defence (Castle (Just lord)) walls)) entr houses)

buildWalls :: City -> Walls -> Either String City
buildWalls city@(City (Just (Defence (Castle lord) Nothing)) entr houses) walls
  | allCitizens city >= 10 =
    Right (City (Just (Defence (Castle lord) (Just walls))) entr houses)
  | otherwise = Left "Not enough citizens"
    where
      allCitizens :: City -> Int
      allCitizens (City _ _ h) = foldl (\x (House citizens) -> x + fromEnum citizens) 0 h
buildWalls _ _ = Left "No lord in city"

--Task 3
data Nat = Z | S Nat

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  Z == Z          = True
  (S x) == (S y)  = x == y
  _ == _          = False

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  Z <= _          = True
  _ <= Z          = False
  (S x) <= (S y)  = x <= y

instance Show Nat where
  show :: Nat -> String
  show = show . fromNat where
    fromNat :: Nat -> Int
    fromNat Z     = 0
    fromNat (S x) = 1 + fromNat x

instance Num Nat where
  (+), (-), (*) :: Nat -> Nat -> Nat
  Z + x     = x
  (S x) + y = S (x + y)

  x - Z         = x
  Z - _         = error "Nat can not be negative"
  (S x) - (S y) = x - y

  Z * _ = Z
  (S x) * y = y + (x * y)

  signum :: Nat -> Nat
  signum Z = Z
  signum _ = S Z

  abs :: Nat -> Nat
  abs = id

  fromInteger :: Integer -> Nat
  fromInteger 0 = Z
  fromInteger x
    | x < 0     = error "Nat must be positive or zero"
    | otherwise = S $ fromInteger (x - 1)

--Task 4
data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Tree a -> Int
size Leaf                = 0
size (Node _ left right) = size left + size right + 1

find :: (Ord a) => Tree a -> a -> Maybe (Tree a)
find Leaf _ = Nothing
find node@(Node x left right) key
  | x == key = Just node
  | key < x = find left key
  | otherwise = find right key

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf key = Node key Leaf Leaf
insert node@(Node x left right) key
  | x == key = node
  | key < x = Node x (insert left key) right
  | otherwise = Node x left (insert right key)

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Leaf
fromList xs = foldl' insert Leaf xs

removeNode :: (Ord a) => Tree a -> a -> Tree a
removeNode Leaf _ = Leaf
removeNode (Node v t1 t2) x
        | x == v = removeRoot (Node v t1 t2)
        | x  < v = Node v (removeNode t2 x) t2
        | otherwise = Node v t1 (removeNode t2 x)

removeRoot :: (Ord a) => Tree a -> Tree a
removeRoot Leaf = Leaf
removeRoot (Node _ Leaf t2) = t2
removeRoot (Node _ t1 Leaf) = t1
removeRoot (Node _ t1 t2) = (Node v2 t1 t2)
    where
        v2 = minElement t2

minElement :: (Ord a) => Tree a -> a
minElement (Node v Leaf _) = v
minElement (Node _ t1 _)   = minElement t1
minElement Leaf            = error "Argument must not be Leaf"

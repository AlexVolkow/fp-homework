module Block3 where

import Control.Applicative (Alternative (..))
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (isPrefixOf)


newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

--Task 1

instance Functor (Parser s) where
    fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
    pure a = Parser $ \s -> Just (a, s)
    (Parser p1) <*> (Parser p2) = Parser $ p1 >=> \(f, s1) -> p2 s1 >>= \(x, s2) -> Just (f x, s2)

instance Monad (Parser s) where
    return = pure
    (Parser p) >>= n = Parser $ \s -> case p s of
            Just (x, xs) -> runParser (n x) xs
            _            -> Nothing


instance Alternative (Parser s) where
    empty = Parser (const Nothing)
    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

--Task 2


ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \s -> case s of
    [] -> Just ((), [])
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \s -> case s of
    []     -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

element :: Eq s => s -> Parser s s
element s = satisfy (== s)

stream :: Eq s => [s] -> Parser s [s]
stream prefix = Parser $ \s -> if prefix `isPrefixOf` s
                                then Just (prefix, snd $ splitAt (length prefix) s)
                                else Nothing

zeroOrMore :: Parser s a -> Parser s [a]
zeroOrMore = many

oneOrMore :: Parser s a -> Parser s [a]
oneOrMore = some

--Task 3

toUnit :: Parser s a -> Parser s ()
toUnit (Parser p) = Parser $ \s -> case p s of
    Just (_, s1) -> Just((), s1)
    Nothing      -> Nothing

parseBrackets :: Parser Char ()
parseBrackets = zeroOrMore parseP *> eof
    where
        parseP = toUnit $ element '(' *> (zeroOrMore parseP) <* element ')'

parseInt :: Parser Char Int
parseInt = parseSign <*> (foldl (\acc x -> acc * 10 + x) 0 <$> oneOrMore number)

number :: Parser Char Int
number = digitToInt <$> satisfy isDigit

parseSign :: Parser Char (Int -> Int)
parseSign = (((-1) *) <$ element '-') <|> id <$ element '+' <|> id <$ ok

--Task 4

parseListOfList :: Parser Char [[Int]]
parseListOfList = ((:) <$> parseList <*> zeroOrMore (elementW ',' *> parseList)) <|> const [] <$> ok <* eof

parseList :: Parser Char [Int]
parseList = (parseIntW <* elementW ',') >>= parseArray

parseArray :: Int -> Parser Char [Int]
parseArray n = skipWhitespace *> impl n <* skipWhitespace
    where
        impl 1   = pure <$> parseIntW
        impl len = (:) <$> (parseIntW <* elementW ',') <*> parseArray (len - 1)

elementW :: Char -> Parser Char Char
elementW s = skipWhitespace *> element s <* skipWhitespace

parseIntW :: Parser Char Int
parseIntW = skipWhitespace *> parseInt <* skipWhitespace

skipWhitespace :: Parser Char String
skipWhitespace = zeroOrMore $ satisfy isSpace

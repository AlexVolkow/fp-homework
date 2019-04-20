module BaseParser
    ( Script
    , Statement(..)
    , StatementValue(..)
    , Parser
    , parseScript
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char (letterChar, alphaNumChar, string, char, space)
import Data.Void
import Data.Char(isSpace)
import qualified Data.Set as Set

data Statement = Assign String [StatementValue] deriving (Show, Eq)

type Script = [Statement]

data StatementValue
    = Text String
    | Reference String
    deriving (Show, Eq)

type Parser = Parsec Void String

parseScript :: Parser Script
parseScript = many parseCommand <* eof

parseCommand :: Parser Statement
parseCommand = space *> parseAssign <* endOfCommand

endOfCommand :: Parser String
endOfCommand = many (satisfy (\x -> isSpace x || x == ';'))

parseAssign :: Parser Statement
parseAssign = (Assign <$> variable) <* char '=' <*> parseStatementValue

parseStatementValue :: Parser [StatementValue]
parseStatementValue = fmap (concat) (some ((fmap (\x -> [x]) parseValue) <|> try(parseProcessable)))

parseValueBase :: Parser StatementValue
parseValueBase = try (Text <$> singleQuote) <|> try (Reference <$> parseReference)

parseValue :: Parser StatementValue
parseValue = parseValueBase <|> try (Text <$> noQuote)

singleQuote :: Parser String
singleQuote = between (char '\'') (char '\'') (many (satisfy (/= '\'')))

parseReference :: Parser String
parseReference = char '$' *> reference

parseProcessable :: Parser [StatementValue]
parseProcessable = try (doubleQuote)

doubleQuote :: Parser [StatementValue]
doubleQuote = between (char '"') (char '"') (parseStatementValueProcessable)
    where
        parseValueProcessable = parseValueBase <|> try (Text <$> noQuoteProcessable)
        parseStatementValueProcessable = fmap (concat) (some ((fmap (\x -> [x]) parseValueProcessable) <|> try(parseProcessable)))

variable :: Parser String
variable = (:) <$> letterChar <*> (many alphaNumChar)

reference :: Parser String
reference = some alphaNumChar

noQuote :: Parser String
noQuote = some ordinary
  where
    ordinary = (try alphaNumChar) <|> escapes <|> satisfy (\x -> (Set.notMember x forbiddenChars) &&  (not (isSpace x)))

noQuoteProcessable :: Parser String
noQuoteProcessable = some ordinary
    where
       ordinary = (try alphaNumChar) <|> escapes <|> satisfy (\x -> x /='"' && x /='$')

forbiddenChars :: Set.Set Char
forbiddenChars = Set.fromList [';', '$', '\'', '"', '`', '|', '(', ')', '\\', '>', '<', '{', '}']

escapes :: Parser Char
escapes =  try ((!!1) <$> string "\\$")
           <|> try (head <$> string "\\\\")
           <|> try ((!!1) <$> string "\\\"")
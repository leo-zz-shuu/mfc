{-# LANGUAGE OverloadedStrings #-}

module Macaf.Scanner.Combinator where

import           Control.Monad              (void)
import           Data.Char                  ()
import           Data.String.Conversions    ()
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (MonadParsec (notFollowedBy, try),
                                             Parsec, between, empty, many,
                                             single, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type MacafParser = Parsec Void Text

sc :: MacafParser ()
sc = L.space space1 lineCmnt empty
  where
    lineCmnt = L.skipLineComment "!"

lexeme :: MacafParser a -> MacafParser a
lexeme = L.lexeme sc

symbol :: Text -> MacafParser Text
symbol = L.symbol sc

parens :: MacafParser a -> MacafParser a
parens = between (symbol "(") (symbol ")")

brackets :: MacafParser a -> MacafParser a
brackets = between (symbol "[") (symbol "]")

braces :: MacafParser a -> MacafParser a
braces = between (symbol "{") (symbol "}")

dquotes :: MacafParser a -> MacafParser a
dquotes = between (single '"') (single '"')

squotes :: MacafParser a -> MacafParser a
squotes = between (single '\'') (single '\'')

semi :: MacafParser ()
semi = void $ symbol ";"

comma :: MacafParser ()
comma = void $ symbol ","

star :: MacafParser ()
star = void $ symbol "*"

pKeyword :: Text -> MacafParser ()
pKeyword keyword = (lexeme . try) (string keyword *> notFollowedBy alphaNumChar)

keywords :: [Text] -- list of reserved keywords
keywords = ["module", "use", "implicit", "integer", "end"]

identifier :: MacafParser Text
identifier = (lexeme . try) (p >>= check)
  where
    p = fmap T.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> single '_')
    check x =
      if x `elem` keywords
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else return x

integer :: MacafParser Integer
integer = lexeme L.decimal

signInteger :: MacafParser Integer
signInteger = L.signed empty L.decimal

bInteger :: MacafParser Integer
bInteger = lexeme (char 'B' >> L.binary)

oInteger :: MacafParser Integer
oInteger = lexeme (char 'O' >> L.octal)

hInteger :: MacafParser Integer
hInteger = lexeme (char 'Z' >> L.hexadecimal)

float :: MacafParser Double
float = lexeme L.float

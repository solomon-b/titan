module Titan.Parser.Token where

import Control.Applicative
import Control.Monad

import Data.Text

import Text.Parser.Combinators (Parsing(..))

import qualified Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer
import qualified Text.Megaparsec.Byte.Lexer
import qualified Text.Parser.Combinators
import qualified Text.Parser.Token

import Titan.Parser.Combinators

between :: Parser bra -> Parser ket -> Parser a -> Parser a
between = Text.Parser.Combinators.between

whitespace :: Parser ()
whitespace = Text.Parser.Token.whiteSpace

symbol :: Text -> Parser Text
symbol = Text.Megaparsec.Byte.Lexer.symbol whitespace

quoted :: Parser a -> Parser a
quoted = between (symbol "\"") (symbol "\"")


lexeme :: Parser a -> Parser a
lexeme = Text.Megaparsec.Char.Lexer.lexeme whitespace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

bracket :: Parser a -> Parser a
bracket = between (symbol "{") (symbol "}")

squareBracket :: Parser a -> Parser a
squareBracket = between (symbol "[") (symbol "]")

angleBracket :: Parser a -> Parser a
angleBracket = between (symbol "<") (symbol ">")

parensOpt :: Parser a -> Parser a
parensOpt p = parens p <|> p

integer :: Parser Integer
integer = lexeme Text.Parser.Token.decimal

comma :: Parser ()
comma = void $ symbol ","

semi :: Parser ()
semi = void $ symbol ";"

colon :: Parser ()
colon = void $ symbol ":"

dot :: Parser ()
dot = void $ symbol "."

fslash :: Parser ()
fslash = void $ symbol "/"

bslash :: Parser ()
bslash = void $ symbol "\\"

arrow :: Parser ()
arrow = void $ symbol "->"

phatArrow :: Parser ()
phatArrow = void $ symbol "=>"

pipe :: Parser ()
pipe = void $ symbol "|"

equal :: Parser ()
equal = void $ symbol "="

rword :: Text -> Parser ()
rword w = (lexeme . try) (Text.Megaparsec.Char.string w *> notFollowedBy Text.Megaparsec.Char.alphaNumChar)

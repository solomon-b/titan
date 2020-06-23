module Titan.Parser.Combinators where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail hiding (fail)

import Data.Text (Text)
import qualified Data.Text
import Data.Void
import Text.Parser.Combinators (try, (<?>))

import qualified Data.Char
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Char
import qualified Text.Parser.Combinators
import qualified Text.Parser.Char
import qualified Text.Parser.Token
import qualified Text.Parser.Token
import qualified Text.Parser.Token.Style

newtype Parser a = Parser { unParser :: Text.Megaparsec.Parsec Void Text a }
  deriving (Functor, Applicative, Alternative, Monad, MonadFail)

type ParseError = Text.Megaparsec.ParseErrorBundle Text Void

runParse :: Parser a -> Text -> Either ParseError a
runParse (Parser p) =
  Text.Megaparsec.runParser p mempty


instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (Parser a) where
    mempty = pure mempty

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

instance Text.Megaparsec.MonadParsec Void Text Parser where
  failure u e               = Parser (Text.Megaparsec.failure u e)
  fancyFailure e            = Parser (Text.Megaparsec.fancyFailure e)
  label l (Parser p)        = Parser (Text.Megaparsec.label l p)
  hidden (Parser p)         = Parser (Text.Megaparsec.hidden p)
  try (Parser p)            = Parser (Text.Megaparsec.try p)
  lookAhead (Parser p)      = Parser (Text.Megaparsec.lookAhead p)
  notFollowedBy (Parser p)  = Parser (Text.Megaparsec.notFollowedBy p)
  withRecovery e (Parser p) = Parser (Text.Megaparsec.withRecovery (unParser . e) p)
  observing (Parser p)      = Parser (Text.Megaparsec.observing p)
  eof                       = Parser Text.Megaparsec.eof
  token f e                 = Parser (Text.Megaparsec.token f e)
  tokens f ts               = Parser (Text.Megaparsec.tokens f ts)
  takeWhileP s f            = Parser (Text.Megaparsec.takeWhileP s f)
  takeWhile1P s f           = Parser (Text.Megaparsec.takeWhile1P s f)
  takeP s n                 = Parser (Text.Megaparsec.takeP s n)
  getParserState            = Parser Text.Megaparsec.getParserState
  updateParserState f       = Parser (Text.Megaparsec.updateParserState f)

instance Text.Parser.Combinators.Parsing Parser where
  try           = Text.Megaparsec.try
  (<?>)         = (Text.Megaparsec.<?>)
  skipMany      = Text.Megaparsec.skipMany
  skipSome      = Text.Megaparsec.skipSome
  unexpected    = fail
  eof           = Parser Text.Megaparsec.eof
  notFollowedBy = Text.Megaparsec.notFollowedBy

instance Text.Parser.Char.CharParsing Parser where
  satisfy :: (Char -> Bool) -> Parser Char
  satisfy = Parser . Text.Megaparsec.satisfy
  char :: Char -> Parser Char
  char    = Text.Megaparsec.Char.char
  notChar :: Char -> Parser Char
  notChar = Text.Megaparsec.Char.char
  anyChar :: Parser Char
  anyChar = Text.Megaparsec.anySingle
  string :: String -> Parser String
  string =  fmap Data.Text.unpack . Text.Megaparsec.Char.string . Data.Text.pack
  text    = Text.Megaparsec.Char.string

instance Text.Parser.Token.TokenParsing Parser where
  someSpace =
      Text.Parser.Token.Style.buildSomeSpaceParser
          (Parser (Text.Megaparsec.skipSome (Text.Megaparsec.satisfy Data.Char.isSpace)))
          Text.Parser.Token.Style.haskellCommentStyle
  highlight _ = id
  semi = Text.Parser.Token.token (Text.Megaparsec.Char.char ';' <?> ";")

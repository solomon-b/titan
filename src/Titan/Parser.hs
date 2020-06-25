module Titan.Parser where

import Control.Applicative
import Control.Monad

import Data.Attoparsec.ByteString
import Data.List.NonEmpty
import Data.ByteString
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import GHC.Word

import Titan.Types

dot :: Parser ()
dot = void $ word8 46

colon :: Parser ()
colon = void $ word8 58

cr :: Parser ()
cr = void $ word8 13

lf :: Parser ()
lf = void $ word8 10

fslash :: Parser ()
fslash = void $ word8 47

alphaNum :: Parser Word8
alphaNum = satisfy $ inClass "a-zA-Z0-9"

sepEndBy1 :: Alternative m => m a -> m sep -> m [a]
sepEndBy1 p sep = (:) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

sepEndBy :: Alternative m => m a -> m sep -> m [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

parseScheme :: Parser ()
parseScheme = do
  void $ string "gemini"
  void $ colon
  void $ fslash
  void $ fslash

parsePath :: Parser [Text]
parsePath = do
  fslash
  path <- sepEndBy (many1 alphaNum) fslash
  pure $ fmap (decodeUtf8 . pack) path

parseDomain :: Parser [Text]
parseDomain = do
  path <- sepEndBy (many1 alphaNum) dot
  pure $ fmap (decodeUtf8 . pack) path

parseUrl :: Parser Url
parseUrl = do
  parseScheme
  domain <- parseDomain
  path <- parsePath <|> pure []
  pure $ Url domain path

--parseRequest :: Parser ([ByteString], [ByteString])
--parseRequest = parseUrl <* cr <* lf <* endOfInput

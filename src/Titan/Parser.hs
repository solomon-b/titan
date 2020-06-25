module Titan.Parser where

import Control.Applicative
import Control.Monad

import Data.Attoparsec.ByteString
import Data.List.NonEmpty
import Data.ByteString

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

parsePath :: Parser [ByteString]
parsePath = fslash *> (fmap pack <$> sepEndBy (many1 alphaNum) fslash)

parseDomain :: Parser [ByteString]
parseDomain = fmap pack <$> sepBy1 (many1 alphaNum) dot

parseUrl :: Parser ([ByteString], [ByteString])
parseUrl = do
  parseScheme
  domain <- parseDomain
  path <- parsePath <|> pure []
  pure (domain, path)

parseRequest :: Parser ([ByteString], [ByteString])
parseRequest = parseUrl <* cr <* lf <* endOfInput

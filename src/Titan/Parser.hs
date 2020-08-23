{-# LANGUAGE TupleSections #-}
module Titan.Parser where

import Control.Applicative
import Control.Monad

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (endOfLine)

import Data.ByteString
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import GHC.Word

import Titan.Types

dot :: Parser ()
dot = void $ word8 46

colon :: Parser ()
colon = void $ word8 58

equal :: Parser ()
equal = void $ word8 61

ampersand :: Parser ()
ampersand = void $ word8 38

qmark :: Parser ()
qmark = void $ word8 63

cr :: Parser ()
cr = void $ word8 13

lf :: Parser ()
lf = void $ word8 10

fslash :: Parser ()
fslash = void $ word8 47

alphaNum :: Parser Word8
alphaNum = satisfy $ inClass "a-zA-Z0-9"

text :: Parser Text
text = decodeUtf8 . pack <$> many1 alphaNum

sepEndBy1 :: Alternative m => m a -> m sep -> m [a]
sepEndBy1 p sep = (:) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

sepEndBy :: Alternative m => m a -> m sep -> m [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

parseScheme :: Parser ()
parseScheme = do
  void $ string "gemini"
  void colon
  void fslash
  void fslash

parsePath :: Parser [Text]
parsePath = fslash *> sepEndBy text fslash

parseQueryParam :: Parser (Text, Text)
parseQueryParam = (,) <$> text <* equal <*> text

parseQueryFlag :: Parser Text
parseQueryFlag = text

parseQueryPFs :: Parser (QueryFlags, QueryParams)
parseQueryPFs = qmark *> (partitionEithers <$> sepBy p ampersand)
  where
    p = (Left <$> parseQueryFlag) <|> (Right <$> parseQueryParam)

parseDomain :: Parser [Text]
parseDomain = sepEndBy text dot

parseRequest :: Parser Request
parseRequest = do
  parseScheme
  domain <- parseDomain
  path <- option [] parsePath
  (flags, params) <- option mempty parseQueryPFs
  endOfLine
  pure $ Request domain path params flags

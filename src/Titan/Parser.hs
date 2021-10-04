{-# LANGUAGE TupleSections #-}
module Titan.Parser where

import Control.Applicative ( Alternative((<|>)))
import Control.Monad ( void )
import Data.Attoparsec.ByteString
    ( inClass,
      satisfy,
      string,
      word8,
      many1,
      option,
      sepBy',
      try,
      endOfInput,
      Parser )
import Data.Attoparsec.ByteString.Char8 (endOfLine)
import Data.ByteString ( pack )
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Word ( Word8 )
import Titan.Types ( Request(Request), QueryFlags, QueryParams )

dot :: Parser ()
dot = void $ word8 46

colon :: Parser ()
colon = void $ word8 58

equal :: Parser ()
equal = void $ word8 61

ampersand :: Parser ()
ampersand = void $ word8 38 -- '&'

qmark :: Parser ()
qmark = void $ word8 63 -- '?'

cr :: Parser ()
cr = void $ word8 13

lf :: Parser ()
lf = void $ word8 10

fslash :: Parser ()
fslash = void $ word8 47

alphaNum :: Parser Word8
alphaNum = satisfy $ inClass "a-zA-Z0-9"

plus :: Parser Word8
plus = word8 43

minus :: Parser Word8
minus = word8 45

plus2b :: Parser Word8
plus2b = do
  _ <- string "%2B"
  pure 43

plusAny :: Parser Word8
plusAny = try plus <|> try plus2b

text :: Parser Text
text = decodeUtf8 . pack <$> many1 alphaNum

textPlusMinus :: Parser Text
textPlusMinus = decodeUtf8 . pack <$> many1 (try alphaNum <|> try plusAny <|> try minus)

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
parsePath = fslash *> sepEndBy textPlusMinus fslash

parseQueryParam :: Parser (Text, Text)
parseQueryParam = (,) <$> text <* equal <*> text

parseQueryFlag :: Parser Text
parseQueryFlag = text

parseQueryPFs :: Parser (QueryFlags, QueryParams)
parseQueryPFs = qmark *> (partitionEithers <$> sep)

sep :: Parser [Either Text (Text,Text)]
sep = sepBy' p ampersand
  where
    p = try (Left <$> (parseQueryFlag <* endOfInput)) <|> try (Right <$> parseQueryParam)

parseDomain :: Parser [Text]
parseDomain = sepEndBy text dot

parseRequest :: Parser Request
parseRequest = do
  parseScheme
  domain <- parseDomain
  path' <- option [] parsePath
  (flags, params) <- option mempty parseQueryPFs
  endOfLine
  pure $ Request domain path' params flags

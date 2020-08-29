{-# LANGUAGE RankNTypes #-}
module Titan.Types where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Text

data ResponseCode =
    One   -- Input
  | Two   -- Success
  | Three -- Redirect
  | Four  -- Temporary Failure
  | Five  -- Permanent Failure
  | Six   -- Client Certificate Required

instance Show ResponseCode where
  show = \case
    One   -> "10"
    Two   -> "20"
    Three -> "30"
    Four  -> "40"
    Five  -> "50"
    Six   -> "60"

data Header = Header
 { _status :: ResponseCode
 , _meta   :: Text
 } deriving Show
makeLenses ''Header

printHeader :: Header -> Text
printHeader (Header status' meta') =
  (pack . show) status' <> " " <> meta' <> "\r\n"

data Response a = Response
  { _header :: Header
  , _body   :: Maybe a
  } deriving (Show, Functor)
makeLenses ''Response

invalidRequest :: String -> Response Text
invalidRequest msg =
  Response (Header Five (pack msg)) Nothing

showResponse :: Response Text -> Text
showResponse (Response header' mbody) =
  printHeader header' <> fromMaybe mempty mbody <> "\r\n"

data Mime = PlainText | Gemini
--    Application
--  | Audio
--  | Example
--  | Font
--  | Image
--  | Model
--  | Text
--  | Video
--  deriving Show

type Domain = [Text]
type Path = [Text]
type QueryParams = [(Text, Text)]
type QueryFlags = [Text]

data Request = Request
  { _request :: Domain
  , _path :: Path
  , _qp :: QueryParams
  , _qf :: QueryFlags
  } deriving Show
makeLenses ''Request

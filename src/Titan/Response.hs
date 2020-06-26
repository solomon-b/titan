module Titan.Response where

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

data Response a = Response
  { _header :: Header
  , _body   :: Maybe a
  } deriving (Show, Functor)

invalidRequestRespond :: Response Text
invalidRequestRespond =
  Response (Header Five "text/plain") (Just "BAD REQUEST")

testResponse :: Response Text
testResponse =
  Response (Header Two "text/plain") (Just "Hello Moon!")

printResponse :: Response Text -> Text
printResponse (Response (Header status meta) mbody) =
  let body = maybe mempty id mbody
  in (pack . show) status <> " " <> meta <> "\r\n" <> body

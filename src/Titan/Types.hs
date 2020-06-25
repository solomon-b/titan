module Titan.Types where

import Data.ByteString
import Data.Text

data ResponseCode =
    One   -- Input
  | Two   -- Success
  | Three -- Redirect
  | Four  -- Temporary Failure
  | Five  -- Permanent Failure
  | Size  -- Client Certificate Required
  deriving Show

data Mime =
    Application
  | Audio
  | Example
  | Font
  | Image
  | Model
  | Text
  | Video
  deriving Show

data Header = Header
 { _status :: ResponseCode
 , _meta   :: Text
 } deriving Show


module Titan.Response where

import           Control.Lens
import           Data.Maybe (fromMaybe)
import           Data.Text

import Titan.Types

-----------------------
--- Stock Responses ---
-----------------------

invalidRequest :: String -> Response Text
invalidRequest msg =
  Response (Header Five (pack msg)) Nothing

testResponse :: Response Text
testResponse =
  Response (Header Two "text/gemini") (Just "Hello Moon!")



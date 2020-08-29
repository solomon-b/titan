module Titan.ToResponse where

import Data.Proxy
import Data.Text (Text, concat, pack)

class ToResponse a mime where
  toResponse :: Proxy mime -> a -> Text

--instance Show a => ToResponse a mime where
--  toResponse _ = pack . show

instance ToResponse Int mime where
  toResponse _ = pack . show

instance ToResponse Text mime where
  toResponse _ = id

instance ToResponse [Text] mime where
  toResponse _ = Data.Text.concat

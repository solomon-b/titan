module Titan.ToResponse where

import Data.Text (Text, concat, pack)

class ToResponse a where
  toResponse :: a -> Text

--instance Show a => ToResponse a mime where
--  toResponse _ = pack . show

instance ToResponse Int where
  toResponse = pack . show

instance ToResponse Text where
  toResponse = id

instance ToResponse [Text] where
  toResponse = Data.Text.concat

module Titan.Types where

import Data.Text

type Domain = Text
type Path = [Text]

data Url = Url Domain Path

module Titan.Types where

import Data.Text


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

type Domain = [Text]
type Path = [Text]

data Url = Url Domain Path
  deriving Show

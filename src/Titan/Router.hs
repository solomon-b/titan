module Titan.Router where

import Debug.Trace

import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import Data.Text (Text, pack, unpack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Web.HttpApiData

import Titan.Types

data Get (a :: Type)

infixr 8 :<|>
data a :<|> b = a :<|> b

infixr 9 :>
data (a :: k) :> (b :: Type)

data Capture (a :: Type)

type Handler = Request -> Either Text (IO (Response Text))

type family Server layout :: Type

type instance Server (Get a) = IO (Response a)

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((_ :: Symbol) :> r) = Server r

type instance Server (Capture a :> r) = a -> Server r

class HasServer layout where
  route :: Proxy layout -> Server layout -> Handler

instance Show a => HasServer (Get a) where
  route :: Proxy (Get a) -> Server (Get a) -> Handler
  route _ handle (Request _ []) = Right $ (fmap . fmap) (pack . show) handle
  route _ _ _ = Left "Extra Url piece(s)"

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Proxy (a :<|> b) -> Server (a :<|> b) -> Handler
  route _ (h1 :<|> h2) req =
    route (Proxy :: Proxy a) h1 req <> route (Proxy :: Proxy b) h2 req

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route ::
    Proxy ((s :: Symbol) :> r) ->
    Server ((s :: Symbol) :> r) ->
    Handler
  route _ h (Request hn (x:xs))
    | symbolVal (Proxy :: Proxy s) == unpack x = route (Proxy :: Proxy r) h (Request hn xs)
  route _ _ _ = Left "Missing Url piece"

instance (FromHttpApiData a, HasServer r) => HasServer (Capture a :> r) where
  route ::
    Proxy (Capture a :> r) ->
    Server (Capture a :> r) ->
    Handler
  route _ h (Request hn (x:xs)) = do
    a <- parseUrlPiece $ traceShowId x
    route (Proxy :: Proxy r) (h a) (Request hn xs)
  route _ _ _ = Left "Missing Url piece"

serve ::
  HasServer layout =>
  Proxy layout -> Server layout -> Request -> IO (Response Text)
serve p s req = do
  case route p s req of
    Right resp -> resp
    Left e -> pure $ invalidRequest $ unpack e

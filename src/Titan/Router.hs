module Titan.Router where

import Control.Applicative
import Data.Proxy
import Data.Time
import Text.Read
import           GHC.TypeLits
import           Data.Text (Text, pack, unpack)
import           Titan.Types

data Get (a :: *)

infixr 8 :<|>
data a :<|> b = a :<|> b

infixr 9 :>
data (a :: k) :> (b :: *)

data Capture (a :: *)

type Handler = Request -> Maybe (IO (Response Text))

type family Server layout :: *

type instance Server (Get a) = IO (Response a)

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((_ :: Symbol) :> r) = Server r

type instance Server (Capture a :> r) = a -> Server r

class HasServer layout where
  route :: Proxy layout -> Server layout -> Handler

instance Show a => HasServer (Get a) where
  route :: Proxy (Get a) -> Server (Get a) -> Handler
  route _ handle req = Just $ (fmap . fmap) (pack . show) handle

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Proxy (a :<|> b) -> Server (a :<|> b) -> Handler
  route _ (h1 :<|> h2) req =
    route (Proxy :: Proxy a) h1 req <|> route (Proxy :: Proxy b) h2 req

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route ::
    Proxy ((s :: Symbol) :> r) ->
    Server ((s :: Symbol) :> r) ->
    Handler
  route _ h (Request hn (x:xs))
    | symbolVal (Proxy :: Proxy s) == unpack x = route (Proxy :: Proxy r) h (Request hn xs)
  route _ _ _ = Nothing

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  route ::
    Proxy (Capture a :> r) ->
    Server (Capture a :> r) ->
    Handler
  route _ h (Request hn (x:xs)) = do
    a <- readMaybe . unpack $ x
    route (Proxy :: Proxy r) (h a) (Request hn xs)

serve ::
  HasServer layout =>
  Proxy layout -> Server layout -> Request -> IO (Response Text)
serve p s req = do
  case route p s req of
    Just resp -> resp
    Nothing -> pure $ invalidRequest "Not Found Error"

-------------------
-- Example route --
-------------------

type MyAPI = "date" :> Get Day
        :<|> "time" :> Capture TimeZone :> Get ZonedTime

handleDate :: IO (Response Day)
handleDate = do
  date <- utctDay <$> getCurrentTime
  pure $ Response (Header Two "text/gemini") (Just date)

handleTime :: TimeZone -> IO (Response ZonedTime)
handleTime tz = do
  time <- utcToZonedTime tz <$> getCurrentTime
  pure $ Response (Header Two "text/gemini") (Just time)

handleMyAPI :: Server MyAPI
handleMyAPI = handleDate :<|> handleTime

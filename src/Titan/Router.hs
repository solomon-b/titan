module Titan.Router where

import Control.Monad.Except

import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import Data.Text (Text, pack)

--import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

--import Web.HttpApiData

import Titan.Types

data Get (a :: Type)

infixr 8 :<|>
data a :<|> b = a :<|> b

infixr 9 :>
data (a :: k) :> (b :: Type)

data Capture (a :: Type)

data RouteMismatch = NotFound
  deriving (Eq, Show)

newtype RouteResult a =
  RR { routeResult :: Either RouteMismatch a }
  deriving (Eq, Show, Semigroup)

type RoutingApplication =
  Request -> (RouteResult (Response Text) -> IO (Response Text)) -> IO (Response Text)

type Application = Request -> (Response Text -> IO (Response Text)) -> IO (Response Text)

toApplication :: RoutingApplication -> Application
toApplication ra request cb = ra request (routingRespond . routeResult)
  where
    routingRespond :: Either RouteMismatch (Response Text) -> IO (Response Text)
    routingRespond (Left NotFound) =
      cb $ Response (Header Five "The requested resource could not be found but may be available in the future.") Nothing
    routingRespond (Right resp) = cb resp

serve ::
  HasServer layout =>
  Proxy layout -> Server layout -> Application
serve p s = toApplication (route p s)

--type instance Server ((_ :: Symbol) :> r) = Server r
--type instance Server (Capture a :> r) = a -> Server r

class HasServer api where
  type Server api :: Type
  route :: Proxy api -> Server api -> RoutingApplication

instance Show a => HasServer (Get a) where
  type Server (Get a) = ExceptT (ResponseCode, Text) IO (Response a)
  route :: Proxy (Get a) -> Server (Get a) -> RoutingApplication
  route _ handler (Request _ []) cb = do
    e <- runExceptT handler
    cb . RR . Right $
      case e of
        Right resp -> pack . show <$> resp
        Left (code, msg) -> Response (Header code msg) Nothing
  route _ _ _ cb = cb . RR . Left $ NotFound

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type Server (a :<|> b) = Server a :<|> Server b
  route :: Proxy (a :<|> b) -> Server (a :<|> b) -> RoutingApplication
  route _ (h1 :<|> h2) req cb =
    route (Proxy :: Proxy a) h1 req $
      \case
        resp1@(RR (Left _)) ->
          route (Proxy :: Proxy b) h2 req $
            \resp2 -> cb (resp1 <> resp2)
        resp -> cb resp

--instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
--  route ::
--    Proxy ((s :: Symbol) :> r) ->
--    Server ((s :: Symbol) :> r) ->
--    Application
--  route _ h (Request hn (x:xs))
--    | symbolVal (Proxy :: Proxy s) == unpack x = route (Proxy :: Proxy r) h (Request hn xs)
--  route _ _ _ = Left "Missing Url piece"
--
--instance (FromHttpApiData a, HasServer r) => HasServer (Capture a :> r) where
--  route ::
--    Proxy (Capture a :> r) ->
--    Server (Capture a :> r) ->
--    Application
--  route _ h (Request hn (x:xs)) = do
--    a <- parseUrlPiece x
--    route (Proxy :: Proxy r) (h a) (Request hn xs)
--  route _ _ _ = Left "Missing Url piece"

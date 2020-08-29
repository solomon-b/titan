module Titan.Router where

import Control.Monad.Except
import Control.Lens

import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import Data.Text (Text, pack, unpack)

import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Web.HttpApiData

import Titan.ToResponse
import Titan.Types

data Get (a :: Type)

infixr 8 :<|>
data a :<|> b = a :<|> b

infixr 9 :>
data (a :: k) :> (b :: Type)

data Capture (a :: Type)

data QueryParam (sym :: Symbol) (a :: Type)

data QueryFlag (sym :: Symbol)

data RouteMismatch = NotFound
  deriving (Eq, Show)

newtype RouteResult a =
  RR { routeResult :: Either RouteMismatch a }
  deriving (Eq, Show, Semigroup)

type RoutingApplication =
  Request -> (RouteResult (Response Text) -> IO (Response Text)) -> IO (Response Text)

type Application = Request -> (Response Text -> IO (Response Text)) -> IO (Response Text)

toApplication :: RoutingApplication -> Application
toApplication ra req cb = ra req (routingRespond . routeResult)
  where
    routingRespond :: Either RouteMismatch (Response Text) -> IO (Response Text)
    routingRespond (Left NotFound) =
      cb $ Response (Header Five "The requested resource could not be found but may be available in the future.") Nothing
    routingRespond (Right resp) = cb resp

serve ::
  HasServer mime layout =>
  Proxy mime -> Proxy layout -> Server mime layout -> Application
serve p p' s = toApplication (route p p' s)


class HasServer mime api where
  type Server mime api :: Type

  route :: Proxy mime -> Proxy api -> Server mime api -> RoutingApplication

instance ToResponse a mime => HasServer mime (Get a) where
  type Server mime (Get a) = ExceptT (ResponseCode, Text) IO (Response a)

  route :: Proxy mime -> Proxy (Get a) -> Server mime (Get a) -> RoutingApplication
  route p _ handler (Request _ [] _ _) cb = do
    e <- runExceptT handler
    cb . RR . Right $
      case e of
        Right resp -> toResponse p <$> resp
        Left (code, msg) -> Response (Header code msg) Nothing
  route _ _ _ _ cb = cb . RR . Left $ NotFound

instance (HasServer mime a, HasServer mime b) =>
         HasServer mime (a :<|> b) where
  type Server mime (a :<|> b) = Server mime a :<|> Server mime b

  route :: Proxy mime -> Proxy (a :<|> b) -> Server mime (a :<|> b) -> RoutingApplication
  route p _ (h1 :<|> h2) req cb =
    route p (Proxy :: Proxy a) h1 req $
      \case
        resp1@(RR (Left _)) ->
          route p (Proxy :: Proxy b) h2 req $
            \resp2 -> cb (resp1 <> resp2)
        resp -> cb resp

instance (KnownSymbol s, HasServer mime r) => HasServer mime ((s :: Symbol) :> r) where
  type Server mime ((s :: Symbol) :> r) = Server mime r

  route ::
    Proxy mime ->
    Proxy ((s :: Symbol) :> r) ->
    Server mime ((s :: Symbol) :> r) ->
    RoutingApplication
  route p _ h req@(Request _ (x:_) _ _) cb
    | symbolVal (Proxy :: Proxy s) == unpack x =
      route p (Proxy :: Proxy r) h (req & over path tail) cb
  route _ _ _ _ cb = cb . RR . Left $ NotFound

instance (FromHttpApiData a, HasServer mime r) => HasServer mime (Capture a :> r) where
  type Server mime (Capture a :> r) = a -> Server mime r

  route ::
    Proxy mime ->
    Proxy (Capture a :> r) ->
    Server mime (Capture a :> r) ->
    RoutingApplication
  route p _ h req@(Request _ (x:_) _ _) cb =
    case parseUrlPiece x of
      Right a -> route p (Proxy :: Proxy r) (h a) (req & over path tail) cb
      Left _ -> cb . RR . Left $ NotFound
  route _ _ _ _ cb = cb . RR . Left $ NotFound

instance (KnownSymbol sym, FromHttpApiData a, HasServer mime r) =>
  HasServer mime (QueryParam sym a :> r) where
  type Server mime (QueryParam sym a :> r) = Maybe a -> Server mime r

  route ::
    Proxy mime ->
    Proxy (QueryParam sym a :> r) ->
    Server mime (QueryParam sym a :> r) ->
    RoutingApplication
  route p _ h req cb =
    let parseParam = (either (const Nothing) Just . parseQueryParam)
        key = pack $ symbolVal (Proxy :: Proxy sym)
        val = lookup key (_qp req) >>= parseParam
    in route p (Proxy :: Proxy r) (h val) req cb

instance (KnownSymbol sym, HasServer mime r) => HasServer mime (QueryFlag sym :> r) where
  type Server mime (QueryFlag sym :> r) = Bool -> Server mime r

  route ::
    Proxy mime ->
    Proxy (QueryFlag sym :> r) ->
    Server mime (QueryFlag sym :> r) ->
    RoutingApplication
  route p _ h req cb =
    let key = pack $ symbolVal (Proxy :: Proxy sym)
        flag = elemOf (qf . folded) key req
    in route p (Proxy :: Proxy r) (h flag) req cb

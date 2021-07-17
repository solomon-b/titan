module Main where

import Control.Monad.Except
    ( MonadIO(liftIO), MonadError(throwError) )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Data.Proxy ( Proxy(..) )
import Data.Time
    ( Day,
      TimeZone,
      UTCTime(utctDay),
      ZonedTime,
      utcToZonedTime,
      getCurrentTime )
import Data.Text (Text, pack)
import System.Environment (getArgs)
import qualified Network.Simple.TCP.TLS as Z
import Web.HttpApiData
    ( readTextData, FromHttpApiData(parseUrlPiece) )
import Titan
    ( makeContext,
      runServer,
      parseArgs,
      type (:<|>)(..),
      type (:>),
      Capture,
      Get,
      HasServer(Server),
      QueryFlag,
      QueryParam,
      ToResponse(..),
      Header(Header),
      Response(Response),
      ResponseCode(Five, Two) )

----------------------
--- Example routes ---
----------------------

type MyAPI = "date" :> Get Day
        :<|> "time" :> Capture Timezone :> Get ZonedTime
        :<|> "add" :> QueryParam "x" Int :> QueryParam "y" Int :> Get Int
        :<|> "books" :> QueryFlag "published" :> Get [Text]

newtype Timezone = Timezone TimeZone
  deriving Read

instance ToResponse Day where
  toResponse a = pack $ show a

instance ToResponse ZonedTime where
  toResponse a = pack $ show a

instance FromHttpApiData Timezone where
  parseUrlPiece t = Timezone <$> (readTextData t :: Either Text TimeZone)

handleDate :: Server (Get Day)
handleDate = do
  date <- liftIO $ utctDay <$> getCurrentTime
  pure $ Response (Header Two "text/gemini") (Just date)

handleTime :: Server (Capture Timezone :> Get ZonedTime)
handleTime (Timezone tz) = do
  time <- liftIO $ utcToZonedTime tz <$> getCurrentTime
  pure $ Response (Header Two "text/gemini") (Just time)

handleAdd :: Server (QueryParam "x" Int :> QueryParam "y" Int :> Get Int)
handleAdd (Just x) (Just y) =
  pure $ Response (Header Two "text/gemini") (Just (x + y))
handleAdd Nothing Nothing = throwError $ (Five, "Bad values for 'x' and 'y'")
handleAdd Nothing _ = throwError $ (Five, "Bad values for 'x'")
handleAdd _ Nothing = throwError $ (Five, "Bad values for 'y'")

handleBook :: Server (QueryFlag "published" :> Get [Text])
handleBook published =
  let books = [ ("One Flew Over the Cuckcoo's Next", True)
              , ("The Autobiography of Alice B. Toklas", True)
              , ("The Dead Sea Scrolls", False)
              , ("The Light House", False)]
  in if published
     then pure $ Response (Header Two "text/gemini") $
          Just $ fst <$> filter (not . snd) books
     else pure $ Response (Header Two "text/gemini") $
          Just $ fmap fst books

handleMyAPI :: Server MyAPI
handleMyAPI = handleDate :<|> handleTime :<|> handleAdd :<|> handleBook

------------
--- Main ---
------------

main :: IO ()
main = Z.withSocketsDo $ do
  args <- getArgs >>= parseArgs
  runReaderT (runServer (Proxy :: Proxy MyAPI) handleMyAPI) (makeContext args)

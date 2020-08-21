module Main where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Proxy
import Data.Time
import Data.Text (Text)
import System.Environment (getArgs)
import qualified Network.Simple.TCP.TLS as Z
import Web.HttpApiData

import Titan

----------------------
--- Example routes ---
----------------------

type MyAPI = "date" :> Get Day
        :<|> "time" :> Capture Timezone :> Get ZonedTime
        :<|> "hello" :> Capture Text :> Get Text

newtype Timezone = Timezone TimeZone
  deriving Read

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

handleHello :: Server (Capture Text :> Get Text)
handleHello name = pure $ Response (Header Two "text/gemini") (Just $ "Hello " <> name)

handleMyAPI :: Server MyAPI
handleMyAPI = handleDate :<|> handleTime :<|> handleHello

------------
--- Main ---
------------

main :: IO ()
main = Z.withSocketsDo $ do
  args <- getArgs >>= parseArgs
  runReaderT (runServer (Proxy :: Proxy MyAPI) handleMyAPI) (makeContext args)

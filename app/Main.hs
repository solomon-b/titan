module Main where

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

handleDate :: IO (Response Day)
handleDate = do
  date <- utctDay <$> getCurrentTime
  pure $ Response (Header Two "text/gemini") (Just date)

handleTime :: Timezone -> IO (Response ZonedTime)
handleTime (Timezone tz) = do
  time <- utcToZonedTime tz <$> getCurrentTime
  pure $ Response (Header Two "text/gemini") (Just time)

handleHello :: Text -> IO (Response Text)
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

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
        :<|> "add" :> QueryParam "x" Int :> QueryParam "y" Int :> Get Int

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

handleAdd :: Server (QueryParam "x" Int :> QueryParam "y" Int :> Get Int)
handleAdd (Just x) (Just y) =
  pure $ Response (Header Two "text/gemini") (Just (x + y))
handleAdd Nothing Nothing = throwError $ (Five, "Bad values for 'x' and 'y'")
handleAdd Nothing _ = throwError $ (Five, "Bad values for 'x'")
handleAdd _ Nothing = throwError $ (Five, "Bad values for 'y'")

handleMyAPI :: Server MyAPI
handleMyAPI = handleDate :<|> handleTime :<|> handleAdd

------------
--- Main ---
------------

main :: IO ()
main = Z.withSocketsDo $ do
  args <- getArgs >>= parseArgs
  runReaderT (runServer (Proxy :: Proxy MyAPI) handleMyAPI) (makeContext args)

module Main where

import Control.Monad.Reader
import Data.Proxy
import Data.Time
import System.Environment (getArgs)
import qualified Network.Simple.TCP.TLS as Z

import Titan

----------------------
--- Example routes ---
----------------------

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

------------
--- Main ---
------------

main :: IO ()
main = Z.withSocketsDo $ do
  args <- getArgs >>= parseArgs
  runReaderT (runServer (Proxy :: Proxy MyAPI) handleMyAPI) (makeContext args)

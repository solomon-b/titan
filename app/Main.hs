module Main where

-- TODO: Set Proper Error Messages in Parser
-- TODO: Two Digit ResponseCodes

import           Control.Monad.Reader
import           Control.Lens

import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)

import           Data.X509.CertificateStore (makeCertificateStore, CertificateStore)


import qualified Network.TLS as T
import qualified Network.Simple.TCP.TLS as Z
import qualified Network.Socket as NS
import           System.Environment (getArgs)
import           Data.Attoparsec.ByteString (parseOnly)
import           Data.Proxy
import           Titan.ArgParser
import           Titan.Parser
import           Titan.Router
import           Titan.Types

data ServerContext = ServerContext
  { _credential  :: T.Credential
  , _hostPref    :: Z.HostPreference
  , _serviceName :: NS.ServiceName
  , _certStore   :: Maybe CertificateStore
  }
makeLenses ''ServerContext

makeContext :: (NS.HostName, Options) -> ServerContext
makeContext (hn, opts) =
  ServerContext (_optServerCredentials opts)
                (Z.Host hn)
                (_optServerPort opts)
                (makeCertificateStore <$> _optCACert opts)

runServer :: HasServer layout =>
  Proxy layout -> Server layout -> ReaderT ServerContext IO ()
runServer p handler = do
  ServerContext cred hp port ycs <- ask
  let ss = Z.makeServerSettings cred ycs
  Z.serve ss hp port $
    \(ctx, caddr) -> do
      putStrLn $ "request received from " <> show caddr
      Z.recv ctx >>= \case
        Nothing -> sendResponse ctx (invalidRequest "Empty Request")
        Just req ->
          case parseOnly parseRequest req of
            Left err -> sendResponse ctx (invalidRequest err)
            Right parsedRequest -> do
              resp <- serve p handler parsedRequest
              sendResponse ctx resp

sendResponse :: Z.Context -> Response Text -> IO ()
sendResponse ctx = Z.send ctx . encodeUtf8 . showResponse

main :: IO ()
main = Z.withSocketsDo $ makeContext <$> (getArgs >>= parseArgs) >>= runReaderT (runServer (Proxy :: Proxy MyAPI) handleMyAPI)


module Titan ( module Titan.ArgParser
             , module Titan.Parser
             , module Titan.Router
             , module Titan.ToResponse
             , module Titan.Types
             , ServerContext(..)
             , runServer
             , makeContext
             ) where

-- TODO: Set Proper Error Messages in Parser
-- TODO: Two Digit ResponseCodes

import Control.Monad.Reader
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.X509.CertificateStore (makeCertificateStore, CertificateStore)
import qualified Network.TLS as T
import qualified Network.Simple.TCP.TLS as Z
import qualified Network.Socket as NS
import Data.Attoparsec.ByteString (parseOnly)
import Data.Proxy

import Titan.ToResponse
import Titan.Types
import Titan.Router
import Titan.Parser
import Titan.ArgParser

data ServerContext = ServerContext
  { _credential  :: T.Credential
  , _hostPref    :: Z.HostPreference
  , _serviceName :: NS.ServiceName
  , _certStore   :: Maybe CertificateStore
  }

makeContext :: (NS.HostName, Options) -> ServerContext
makeContext (hn, opts) =
  ServerContext (_optServerCredentials opts)
                (Z.Host hn)
                (_optServerPort opts)
                (makeCertificateStore <$> _optCACert opts)

runServer :: HasServer 'PlainText layout =>
  Proxy layout -> Server 'PlainText layout -> ReaderT ServerContext IO ()
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
              resp <- serve (Proxy :: Proxy 'PlainText) p handler parsedRequest pure
              sendResponse ctx resp
  where
    sendResponse :: Z.Context -> Response Text -> IO ()
    sendResponse ctx = Z.send ctx . encodeUtf8 . showResponse

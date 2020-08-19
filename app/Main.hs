{-# LANGUAGE EmptyCase #-}
module Main where

-- TODO: Set Proper Error Messages in Parser
-- TODO: Two Digit ResponseCodes
-- TODO: Router

import           Control.Lens
import           Data.Char (toUpper)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.X509 (SignedCertificate)
import           Data.X509.CertificateStore (makeCertificateStore, CertificateStore)
import           Data.X509.File (readSignedObject)
import qualified Data.ByteString.Char8 as B
import qualified Network.TLS as T
import qualified Network.Simple.TCP.TLS as Z
import qualified Network.Socket as NS
import           System.Console.GetOpt
import           System.Environment (getProgName, getArgs)

import           Data.Attoparsec.ByteString (parseOnly)
import           Titan.Parser
import           Titan.Response
import           Titan.Types

data Options = Options
  { _optServerPort         :: NS.ServiceName
  , _optServerCertFile     :: FilePath
  , _optServerKeyFile      :: FilePath
  , _optServerCredentials  :: T.Credential
  , _optCACert             :: Maybe [SignedCertificate]
  }
makeLenses ''Options

server :: T.Credential -> Z.HostPreference -> NS.ServiceName -> Maybe CertificateStore -> IO ()
server cred hp port ycs =
  let ss = Z.makeServerSettings cred ycs
  in Z.serve ss hp port $
    \(ctx, caddr) -> do
      putStrLn $ "request received from " <> show caddr
      Z.recv ctx >>= \case
        Nothing -> sendResponse ctx (invalidRequest "Empty Request")
        Just msg ->
          case parseOnly parseRequest msg of
            Left err -> sendResponse ctx (invalidRequest err)
            Right (Url hostname path) -> sendResponse ctx testResponse

sendResponse :: Z.Context -> Response Text -> IO ()
sendResponse ctx = Z.send ctx . encodeUtf8 . showResponse

main :: IO ()
main = Z.withSocketsDo $ do
  args <- getArgs
  case getOpt RequireOrder options args of
    (actions, [hostname], _) -> do
      opts <- foldl (>>=) (return defaultOptions) actions
      server (_optServerCredentials opts) (Z.Host hostname) (_optServerPort opts)
             (makeCertificateStore <$> _optCACert opts)
    (_, _, msgs) -> do
      pn <- getProgName
      let header = "Usage: " <> pn <> " [OPTIONS] HOSTNAME"
      error $ concat msgs ++ usageInfo header options

defaultOptions :: Options
defaultOptions = Options
  { _optServerPort        = "1965"
  , _optServerCertFile    = error "Missing optServerCertFile"
  , _optServerKeyFile     = error "Missing optServerKeyFile"
  , _optServerCredentials = undefined
  , _optCACert = Nothing
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option [] ["port"] (ReqArg readPort "PORT") "Server Port"
  , Option [] ["cert"] (ReqArg readServerCert "FILE") "Server Certificate"
  , Option [] ["key"]  (ReqArg readServerCredentials "FILE") "Server private key"
  , Option [] ["cacert"] (OptArg readCACert "FILE")
    "CA certificate to verify a client certificate, if given"
  ]

readServerCert :: FilePath -> Options -> IO Options
readServerCert fp opt = pure $ set optServerCertFile fp opt

readServerCredentials :: FilePath -> Options -> IO Options
readServerCredentials arg opt =
  T.credentialLoadX509 (_optServerCertFile opt) arg >>= \case
    Left err -> error err
    Right c ->
      pure $ opt & optServerCredentials .~ c
                 & optServerKeyFile .~ arg

readPort :: NS.ServiceName -> Options -> IO Options
readPort arg opt = pure $ opt & optServerPort .~ arg

readCACert :: Maybe FilePath -> Options -> IO Options
readCACert Nothing    opt = return opt
readCACert (Just arg) opt = do
    certs <- readSignedObject arg
    pure $ opt & optCACert . _Just .~ certs

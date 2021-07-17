module Titan.ArgParser where

import           Control.Lens
import           Data.X509 (SignedCertificate)
import           Data.X509.File (readSignedObject)
import qualified Network.TLS as T
import qualified Network.Socket as NS

import           System.Console.GetOpt
import           System.Environment (getProgName)

data Options = Options
  { _optServerPort         :: NS.ServiceName
  , _optServerCertFile     :: FilePath
  , _optServerKeyFile      :: FilePath
  , _optServerCredentials  :: T.Credential
  , _optCACert             :: Maybe [SignedCertificate]
  }
  deriving Show

makeLenses ''Options

parseArgs :: [String] -> IO (NS.HostName, Options)
parseArgs args =
  case getOpt RequireOrder options args of
    (actions, [hostname], _) -> do
      opts <- foldl (>>=) (return defaultOptions) actions
      pure (hostname, opts)
    (_, _, msgs) -> do
      pn <- getProgName
      let header = "Usage: " <> pn <> " [OPTIONS] HOSTNAME"
      error $ concat msgs ++ usageInfo header options

defaultOptions :: Options
defaultOptions = Options
  { _optServerPort        = "1965"
  , _optServerCertFile    = "./certs/ca.example.com.crt" -- error "Missing optServerCertFile"
  , _optServerKeyFile     = "./certs/ca.example.com.key" -- error "Missing optServerKeyFile"
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

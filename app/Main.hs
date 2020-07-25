{-# LANGUAGE EmptyCase #-}
module Main where

--import Control.Concurrent (forkFinally)
--import qualified Control.Exception as E
--import Control.Monad (unless, forever, void)
--import Data.ByteString
--
--import qualified Data.Text as T (pack)
--import Data.Text.Encoding (encodeUtf8)
--
--import Network.Socket hiding (recv)
--import Network.Socket.ByteString (recv, sendAll)
--
--import Data.Attoparsec.ByteString (parseOnly)
--import Titan.Parser
--import Titan.Response

import           Control.Lens
import           Data.Char (toUpper)
import           Data.X509 (SignedCertificate)
import           Data.X509.CertificateStore (makeCertificateStore, CertificateStore)
import           Data.X509.File (readSignedObject)
import qualified Data.ByteString.Char8 as B
import qualified Network.TLS as T
import qualified Network.Simple.TCP.TLS as Z
import qualified Network.Socket as NS
import           System.Console.GetOpt
import           System.Environment (getProgName, getArgs)

--main :: IO ()
--main = runTCPServer Nothing "1965" talk
--  where
--    talk s = do
--      msg <- recv s 1024
--      unless (Data.ByteString.null msg) $ do
--        let resp = either (const invalidRequestRespond)
--                          (const testResponse)
--                          (parseOnly parseUrl msg)
--        sendAll s $ encodeUtf8 $ printResponse resp
--        talk s

--runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
--runTCPServer mhost port server = withSocketsDo $ do
--  addr <- resolve
--  E.bracket (open addr) close loop
--  where
--    resolve =
--      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
--      in Prelude.head <$> getAddrInfo (Just hints) mhost (Just port)
--    open addr = do
--      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--      setSocketOption sock ReuseAddr 1
--      bind sock (addrAddress addr)
--      let fd = fdSocket sock
--      setCloseOnExecIfNeeded fd
--      listen sock 10
--      pure sock
--    loop sock = forever $ do
--      (conn, peer) <- accept sock
--      Prelude.putStrLn $ "Connection from " ++ show peer
--      void $ forkFinally (server conn) (const $ close conn)



data Options = Options
  { _optServerCertFile     :: FilePath
  , _optServerKeyFile      :: FilePath
  , _optServerCredentials  :: T.Credential
  , _optCACert             :: Maybe [SignedCertificate]
  }
makeLenses ''Options

server :: T.Credential -> Z.HostPreference -> NS.ServiceName -> Maybe CertificateStore -> IO ()
server cred hp port ycs = do
  let ss = Z.makeServerSettings cred ycs
  Z.serve ss hp port $ \(ctx, caddr) -> do
    putStrLn $ show caddr <> " joined."
    consume ctx $ Z.send ctx . B.map toUpper
    putStrLn $ show caddr <> " quit."
  pure ()

-- | Repeatedly receive data from the given 'T.Context' until exhausted,
-- performing the given action on each received chunk.
  where consume :: T.Context -> (B.ByteString -> IO ()) -> IO ()
        consume ctx k =
          Z.recv ctx >>= \case
            Nothing -> return ()
            Just bs -> k bs >> consume ctx k

main :: IO ()
main = Z.withSocketsDo $ do
  args <- getArgs
  case getOpt RequireOrder options args of
    (actions, [hostname, port], _) -> do
      opts <- foldl (>>=) (return defaultOptions) actions
      server (_optServerCredentials opts) (Z.Host hostname) port
             (makeCertificateStore <$> _optCACert opts)
    (_, _, msgs) -> do
      pn <- getProgName
      let header = "Usage: " <> pn <> " [OPTIONS] HOSTNAME PORT"
      error $ concat msgs ++ usageInfo header options

defaultOptions :: Options
defaultOptions = Options
  { _optServerCertFile    = error "Missing optServerCertFile"
  , _optServerKeyFile     = error "Missing optServerKeyFile"
  , _optServerCredentials = undefined
  , _optCACert = Nothing
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option [] ["cert"] (ReqArg readServerCert "FILE") "Server Certificate"
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

readCACert :: Maybe FilePath -> Options -> IO Options
readCACert Nothing    opt = return opt
readCACert (Just arg) opt = do
    certs <- readSignedObject arg
    pure $ opt & optCACert . _Just .~ certs

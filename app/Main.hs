module Main where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Data.ByteString

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    talk s = do
      msg <- recv s 1024
      unless (Data.ByteString.null msg) $ do
        sendAll s msg
        talk s

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve =
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      in Prelude.head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      let fd = fdSocket sock
      setCloseOnExecIfNeeded fd
      listen sock 10
      pure sock
    loop sock = forever $ do
      (conn, peer) <- accept sock
      Prelude.putStrLn $ "Connection from " ++ show peer
      void $ forkFinally (server conn) (const $ close conn)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Chan(Chan,newChan, writeChan, readChan)
-- Perhaps TChan is better
import Control.Exception(bracket, bracketOnError)
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as ByteString
import Data.ByteString(ByteString)
import Data.Text(Text)
import Data.Maybe(fromMaybe)
import qualified Data.Text as Text
import Text.Printf
import Data.Text.Encoding(decodeUtf8Lenient)
import Network.Socket
import Network.Socket.ByteString(recv, sendAll)

type BufferSize = Int

bufferSize :: BufferSize
bufferSize = 1024

type ClientId = String

data Client = Client {clientId :: ClientId,
                      clientConnection :: Socket,
                      clientSocketAddr :: SockAddr
                     } deriving (Eq, Show)


data Message = ClientConnected Client
             | ClientDisconnected ClientId
             | Content Text ClientId
main :: IO ()
main = runTCPServer Nothing "3000" mainClient

mainClient :: (Socket,SockAddr) -> Chan Message-> IO a
mainClient (sock, sockAddr) chan = do
  (cHost,cPort) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV]  True True sockAddr
  let cId = fromMaybe "" cHost ++ (":" ++ fromMaybe "" cPort)
       
        
  let initialMsg = ClientConnected $ Client { clientId = cId, clientConnection = sock, clientSocketAddr = sockAddr }
  writeChan chan initialMsg
  forever $ do
    msg <- receiveMsg bufferSize sock
    writeChan chan $ Content msg cId



mainServer :: Chan Message -> IO ()
mainServer chan = void $ do
  msg <- readChan chan
  case msg of
    ClientConnected client -> printf "Client %s connected, with details %s" (clientId client) (show client)
    ClientDisconnected cId -> printf "Client %s disconnected" cId
    Content content cId -> printf "Client %s: %s" cId (Text.unpack content)


receiveMsg :: BufferSize -> Socket -> IO Text
receiveMsg size sock = go size sock ByteString.empty
  where
    go :: BufferSize -> Socket -> ByteString -> IO Text
    go size sock result = do
          buffer <- recv sock size
          if ByteString.null buffer
          then return $ decodeUtf8Lenient result 
          else go size sock (result <> buffer)
                              
  
runTCPServer :: Maybe HostName -> ServiceName -> ((Socket,SockAddr)-> Chan Message -> IO a) -> IO a
runTCPServer mhost port server = do
  chan <- newChan
  forkFinally (mainServer chan) (\case {Left some -> print some ; Right _ -> putStrLn "Server side channel process finished" }
                                   )
  addr <- resolve
  bracket (open addr) close (loop chan)


  where
    resolve = do
      let hints = defaultHints {
            addrFlags = [AI_PASSIVE],
            addrSocketType = Stream
                               }
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock bufferSize
      return sock
    loop chan sock = forever $ bracketOnError (accept sock) (close . fst)
      $ \s@(conn,_) -> forkFinally (server s chan) (const $ gracefulClose conn 5000) 

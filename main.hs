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
import Data.Text.Encoding(decodeUtf8Lenient)
import Data.Text(Text)
import Data.Maybe(fromMaybe)
import qualified Data.Text as Text
import Text.Printf
import System.IO
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

mainClient :: (Socket,SockAddr) -> Chan Message-> IO ()
mainClient (sock, sockAddr) chan = do
  (cHost,cPort) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV]  True True sockAddr
  let cId = fromMaybe "" cHost ++ (":" ++ fromMaybe "" cPort)    
  let initialMsg = ClientConnected $ Client { clientId = cId, clientConnection = sock, clientSocketAddr = sockAddr }
  writeChan chan initialMsg
  loop cId

  where   
          loop cId = do
            msg <- recv sock bufferSize
            if ByteString.length msg == 0
              then writeChan chan $ ClientDisconnected cId
             else do
              writeChan chan $ Content (decodeUtf8Lenient msg) cId
              loop cId



mainServer :: Chan Message -> IO ()
mainServer chan = forever $ do
  msg <- readChan chan
  hSetBuffering stdout NoBuffering
  case msg of
    ClientConnected client -> printf "Client %s connected, with details %s\n" (clientId client) (show client)
    ClientDisconnected cId -> printf "Client %s disconnected \n" cId
    Content content cId -> printf "Client %s: %s" cId (Text.unpack content)


                              
  
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

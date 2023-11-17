{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Chan(Chan,newChan, writeChan, readChan)
-- Perhaps TChan is better
import Control.Concurrent.MVar
import Control.Exception(bracket, bracketOnError)
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as ByteString
import Data.ByteString(ByteString)
import Data.Text.Encoding(decodeUtf8Lenient)
import Data.Text(Text)
import Data.Maybe(fromMaybe)
import Data.Map(Map)
import qualified Data.Map as Map
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

sendProcess :: MVar (Map ClientId Client)  -> Chan Text -> IO ()
sendProcess mMap msgChan = do
  msg <- readChan msgChan
  clients <- readMVar mMap
  let clientList = Map.toList clients
      clientConnections = map (\(_,client)-> clientConnection client ) clientList
--  sendAll clientList msg
  printf "sending message: %s\n" (Text.unpack msg)
  sendProcess mMap msgChan


mainServer :: MVar(Map ClientId Client) -> Chan Message -> Chan Text-> IO ()
mainServer mClients chan msgChan =  do
  msg <- readChan chan
  hSetBuffering stdout NoBuffering
  case msg of
    ClientConnected client -> do
      printf "Client %s connected, with details %s\n" (clientId client) (show client)
      clients <- takeMVar mClients
      let newClients = Map.insert (clientId client) client clients
      putMVar mClients newClients
      mainServer mClients chan msgChan
    ClientDisconnected cId -> do
      printf "Client %s disconnected \n" cId
      clients <- takeMVar mClients
      let newClients = Map.delete cId clients 
      putMVar mClients newClients
      mainServer mClients chan msgChan
    Content content cId -> do
      --printf "Client %s: %s" cId (Text.unpack content)
      --sendAll content clients
      writeChan msgChan content
      mainServer mClients chan msgChan


                              
  
runTCPServer :: Maybe HostName -> ServiceName -> ((Socket,SockAddr)-> Chan Message -> IO a) -> IO a
runTCPServer mhost port server = do
  chan <- newChan
  msgChan <- newChan
  mMap <- newEmptyMVar
  let clients = Map.empty
  putMVar mMap clients
  forkFinally (mainServer mMap chan msgChan) (\case {Left some -> print some ; Right _ -> putStrLn "Server side channel process finished" }
                                   )
  forkFinally (sendProcess mMap msgChan) (\case {Left some -> print some ; Right _ -> putStrLn "Server side echoing process finished" })
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

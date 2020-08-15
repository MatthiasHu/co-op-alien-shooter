module Main where

import qualified Network.WebSockets as WS
import System.IO.Error
import System.Environment
import System.Exit
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import Data.ByteString.Builder
import qualified Data.Map as Map
import qualified Data.Set as Set

import GameLogic
import Serialization


type ClientID = Int

data ServerState = ServerState
  { clients :: Map.Map ClientID WS.Connection
  , nextClientID :: ClientID
  , game :: Game
  }

initialServerState :: ServerState
initialServerState = ServerState Map.empty 0 startGame

main :: IO ()
main = do
  putStrLn "ok, let's do this..."
  state <- newMVar initialServerState
  WS.runServer addr port (app state)
  where
    addr = "127.0.0.1"
    port = 58436

app :: MVar ServerState -> WS.ServerApp
app state pendingConn = do
  let path = WS.requestPath (WS.pendingRequest pendingConn)
  putStrLn $ "accepting request for " ++ show path
  conn <- WS.acceptRequest pendingConn
  clientID <- addClient state conn
  putStrLn $ "(client " ++ show clientID ++ ")"
  ( do
    sendTestData conn
    listenTo conn )
    `finally` removeClient state clientID

sendTestData ::
  WS.Connection -> IO ()
sendTestData conn =
    WS.sendBinaryData conn
  . toLazyByteString
  . foldMap serializeDrawCommand
  $ [ DrawCommand 0 (Vec 0 0) ]

-- Add new client to the list and return the new id.
addClient :: MVar ServerState -> WS.Connection -> IO ClientID
addClient state conn = modifyMVar state $ \s -> return $
  let id = nextClientID s
      clients' = Map.insert id conn (clients s)
  in (s { clients = clients', nextClientID = id+1 }, id)

-- Remove client from list.
removeClient :: MVar ServerState -> ClientID -> IO ()
removeClient state id = do
  putStrLn ("removing client " ++ show id)
  modifyMVar_ state $ \s -> return $
    let clients' = Map.delete id (clients s)
    in s { clients = clients' }

listenTo :: WS.Connection -> IO ()
listenTo conn = forever $ do
  (WS.receiveData conn :: IO BS.ByteString)

-- broadcastData :: ServerState -> SyncObject -> BS.ByteString -> IO ()
-- broadcastData s so syncData =
--   let clientsMap = Map.findWithDefault Map.empty so (clients s)
--   in forM_ (Map.elems clientsMap) $ \conn ->
--     sendBinaryData conn syncData

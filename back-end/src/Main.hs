module Main where

import qualified Network.WebSockets as WS
import System.IO.Error
import System.Environment
import System.Exit
import Control.Concurrent
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
  forkIO (forever $ gameLoop state >> threadDelay 30000)
  WS.runServer addr port (handleRequests state)
  where
    addr = "127.0.0.1"
    port = 58436

gameLoop :: MVar ServerState -> IO ()
gameLoop state = do
  s <- modifyMVar state $ \s -> return $
    if Map.null (clients s)
    then (s, s)
    else let s' = s { game = tickGame (const noPlayerInput) (game s) }
         in (s', s')
  let drawCommands = drawGame (game s)
  forM_ (Map.elems (clients s)) $ \conn -> do
    sendDrawCommands conn drawCommands

handleRequests :: MVar ServerState -> WS.ServerApp
handleRequests state pendingConn = do
  let path = WS.requestPath (WS.pendingRequest pendingConn)
  putStrLn $ "accepting request for " ++ show path
  conn <- WS.acceptRequest pendingConn
  clientID <- addClient state conn
  putStrLn $ "(client " ++ show clientID ++ ")"
  ( do
    listenTo conn )
    `finally` removeClient state clientID

sendDrawCommands ::
  WS.Connection -> [DrawCommand] -> IO ()
sendDrawCommands conn commands =
    WS.sendBinaryData conn
  . toLazyByteString
  . foldMap serializeDrawCommand
  $ commands

-- Add new client to the list and return the new id.
addClient :: MVar ServerState -> WS.Connection -> IO ClientID
addClient state conn = modifyMVar state $ \s -> return $
  let id = nextClientID s
      clients' = Map.insert id conn (clients s)
      game' = if Map.null (clients s) then startGame else game s
  in ( ServerState
       { clients = clients'
       , nextClientID = id+1
       , game = game'
       }
     , id
     )

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

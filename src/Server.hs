-- |
-- Module      : Server
-- Description : WebSocket interface to the Snake game
-- Copyright   : (c) Fabien Boucher, 2022
-- License     : MIT
-- Maintainer  : fabien.dot.boucher@gmail.com
--
-- This module contains a WS server and a protocol to allow WS client
-- to interact with the Snake game
module Server
  ( -- * Protocol Types
    ProtoMessage (..),
    Direction (..),
    Item (..),
    World (..),

    -- * Facilities data types
    NetworkAddr (..),

    -- * Read message on the WS
    getProtoMessage,

    -- * Functions to start the server
    runServer,
    runServerLocal,
  )
where

import Codec.Serialise
  ( Serialise,
    deserialiseOrFail,
    serialise,
  )
import Control.Concurrent
  ( MVar,
    modifyMVar_,
    newMVar,
    readMVar,
    threadDelay,
  )
import Control.Concurrent.Async (concurrently_)
import Control.Exception (throwIO, try)
import Control.Monad (when)
import qualified Data.Text as T
import GHC.Generics (Generic)
import LeaderBoard
  ( Board,
    LeaderBoard,
    loadLeaderBoard,
    readLeaderBoard,
    writeLeaderBoard,
  )
import qualified Network.WebSockets as WS
import Snake
import System.Log.FastLogger
  ( LogStr,
    LogType' (LogNone),
    TimedFastLogger,
    ToLogStr (toLogStr),
    newTimeCache,
    newTimedFastLogger,
    simpleTimeFormat,
  )
import Witch
import Prelude

logMsg :: TimedFastLogger -> T.Text -> IO ()
logMsg logger msg = logger (\time -> toLogStr (show time) <> " " <> toLogStr msg <> "\n")

-- Protocol used on the WebSocket
---------------------------------

-- | Message of the Protocol
data ProtoMessage
  = -- | Handcheck message
    Hello T.Text
  | -- | Set snake direction message
    SnakeDirection Direction
  | -- | Tick message (propagate the current game state representation)
    Tick World
  | -- | LeaderBoard message (propagate the current leaderBoard to the Client)
    LeaderBoard Board
  | -- | Bye message
    Bye
  deriving (Show, Generic)

instance Serialise ProtoMessage

-- | Read a 'ProtoMessage' message on the WS
getProtoMessage :: WS.Connection -> IO ProtoMessage
getProtoMessage conn = do
  receivedDataE <- try $ WS.receiveData conn
  case receivedDataE of
    Right cborData -> case deserialiseOrFail cborData of
      Right m -> pure m
      Left err -> error $ "Protocol violation. Unabled to decode message: " <> show err
    Left (WS.CloseRequest _code _msg) -> pure Bye
    Left exc@WS.ConnectionClosed -> do
      putStrLn "Received connection closed"
      throwIO exc
    Left unHandledException -> do
      putStrLn $ "Received unhandled exception " <> show unHandledException
      throwIO unHandledException

-- Various types and functions to handle the Server state
---------------------------------------------------------

type ClientID = T.Text

type Clients = [ClientID]

-- | The server state
data ServerState = ServerState
  { clients :: MVar Clients,
    leaderBoard :: LeaderBoard
  }

-- | Create a new server state
newServerState :: IO (Either String ServerState)
newServerState = do
  leaderBoardE <- loadLeaderBoard
  clients <- newMVar []
  pure $ ServerState clients <$> leaderBoardE

-- | Check client exists
clientExists :: ClientID -> ServerState -> IO Bool
clientExists client st = do
  cls <- readMVar $ clients st
  pure $ client `elem` cls

-- | Add a new client to the Server state
addClient :: ClientID -> ServerState -> IO ()
addClient client st = do
  modifyMVar_ (clients st) $ \cls -> do
    pure $ client : cls

-- | Remove a client from the Server state
removeClient :: ClientID -> ServerState -> IO ()
removeClient client st = do
  modifyMVar_ (clients st) $ \cls -> do
    pure $ filter (/= client) cls

addScore :: TimedFastLogger -> LeaderBoard -> ClientID -> Int -> IO ()
addScore logger board ident score = do
  logMsg logger "b"
  wb <- writeLeaderBoard board (ident, score)
  logMsg logger "a"
  case wb of
    Right x -> pure x
    Left err -> logMsg logger $ from ("Unable to save score: " <> show err)

-- | The connection handler
application :: TimedFastLogger -> ServerState -> WS.ServerApp
application logger st pending = do
  conn <- WS.acceptRequest pending
  logMsg logger "Received incoming connection"
  WS.withPingThread conn 30 (pure ()) $ do
    msg <- getProtoMessage conn
    case msg of
      Hello ident -> handleClient ident conn
      _ -> logMsg logger "Protocol violation. Expected Hello."
  where
    sendLeaderBoard :: WS.Connection -> IO ()
    sendLeaderBoard conn = do
      board <- readLeaderBoard $ leaderBoard st
      WS.sendBinaryData conn $ serialise $ LeaderBoard board

    handleClient :: ClientID -> WS.Connection -> IO ()
    handleClient client conn = do
      logMsg logger ("Incoming client: " <> client)
      cExists <- clientExists client st
      if cExists
        then do
          logMsg logger "Client exists. Sending Bye."
          WS.sendClose conn $ serialise Bye
        else do
          logMsg logger "Client new. Sending Welcome."
          WS.sendBinaryData conn $ serialise $ Hello client
          addClient client st
          sendLeaderBoard conn
          handleGame client conn

    handleGame :: ClientID -> WS.Connection -> IO ()
    handleGame client conn = do
      wStateM <- initAppMem
      -- TODO: add cleanup function for when server stop
      -- a sendClose is sent to all clients
      concurrently_
        (handleInputCommands wStateM)
        (handleGameState wStateM)
      where
        initialTickDelay = 500000
        handleInputCommands :: AppMem -> IO ()
        handleInputCommands appMem = do
          resp <- getProtoMessage conn
          case resp of
            SnakeDirection dir -> do
              setDirection appMem dir
              logMsg logger $ "Got SnakeDirection from " <> client
            Bye -> do
              logMsg logger $ "Got Bye message from " <> client
              removeClient client st
              error "End" -- Force both threads to end
            _ -> logMsg logger $ "Unexpected command from " <> client
          handleInputCommands appMem
        handleGameState :: AppMem -> IO ()
        handleGameState appMem = do
          (world, status, speedFactor, score) <- runStep appMem
          WS.sendBinaryData conn $ serialise $ Tick world
          logMsg logger $ "Sending tick to client " <> client
          when (status == GAMEOVER) $ do
            addScore logger (leaderBoard st) client score
            sendLeaderBoard conn
            resetAppMem appMem
          let minDelay = 200000
              delay = max minDelay $ truncate $ initialTickDelay / speedFactor
          logMsg logger $ from ("Waiting " <> show delay <> " for client " <> from client)
          threadDelay delay
          handleGameState appMem

-- Functions to start the server
--------------------------------

data NetworkAddr = NetworkAddr
  { nAddr :: String,
    nPort :: Int
  }
  deriving (Show)

-- | Run a local server on port 9160
runServerLocal :: IO ()
runServerLocal = runServer (NetworkAddr "127.0.0.1" 9160) LogNone

-- | Run a server
runServer :: NetworkAddr -> LogType' LogStr -> IO ()
runServer NetworkAddr {..} logType = do
  serverState <- newServerState
  case serverState of
    Left err -> putStrLn $ "$ Unable to initialize FreeSnaky due to: " <> from err
    Right st -> do
      timeCache <- newTimeCache simpleTimeFormat
      (logger, _) <- newTimedFastLogger timeCache logType
      WS.runServer nAddr nPort $ application logger st

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

    -- * Read message on the WS
    getProtoMessage,

    -- * Functions to start the server
    runServer,
    runLocalServer,
  )
where

import Control.Concurrent as C
  ( modifyMVar_,
    newMVar,
    readMVar,
    threadDelay,
  )
import Control.Concurrent.Async (concurrently_)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Network.WebSockets as WS
import Relude
import Say (say)
import Snake

-- Protocol used on the WebSocket
---------------------------------

-- | Message of the Protocol
data ProtoMessage
  = -- | Handcheck message
    Hello Text
  | -- | Set snake direction message
    SnakeDirection Direction
  | -- | Tick message (propagate the current game state representation)
    Tick World
  | -- | Bye message
    Bye
  deriving (Show, Generic)

instance ToJSON ProtoMessage

instance FromJSON ProtoMessage

-- | Read a 'ProtoMessage' message on the WS
getProtoMessage :: WS.Connection -> IO ProtoMessage
getProtoMessage conn = do
  jsonMsg <- WS.receiveData conn
  case decode jsonMsg of
    Just msg -> pure msg
    Nothing -> error "Protocol violation. Unabled to decode message."

-- Various types and functions to handle the Server state
---------------------------------------------------------

type ClientID = Text

type ServerState = [ClientID]

-- | Create a new server state
newServerState :: ServerState
newServerState = []

-- | Check client exists
clientExists :: ClientID -> ServerState -> Bool
clientExists = elem

-- | Add a new client to the Server state
addClient :: ClientID -> ServerState -> ServerState
addClient client clients = client : clients

-- | A logger function
logText :: Text -> IO ()
logText = say

-- Functions to start start the server
--------------------------------------

-- | Run a local server on port 9160
runLocalServer :: IO ()
runLocalServer = runServer "127.0.0.1" 9160

-- | Run a server
runServer :: Text -> Int -> IO ()
runServer addr port = do
  s <- C.newMVar newServerState
  WS.runServer (toString addr) port $ application s

-- | The connection handler
application :: MVar ServerState -> WS.ServerApp
application stM pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    msg <- getProtoMessage conn
    case msg of
      Hello ident -> handleClient ident conn
      _ -> logText "Protocol violation. Expected Hello."
  where
    handleClient :: ClientID -> WS.Connection -> IO ()
    handleClient client conn = do
      clients <- C.readMVar stM
      logText $ "Incomming client: " <> client
      if clientExists client clients
        then do
          logText "Client exists. Sending Bye."
          WS.sendTextData conn $ encode Bye
        else do
          modifyMVar_ stM $ \st -> do
            logText "Client new. Sending Welcome."
            WS.sendTextData conn $ encode $ Hello client
            pure $ addClient client st
          handleGame client conn

    handleGame :: ClientID -> WS.Connection -> IO ()
    handleGame client conn = do
      wStateM <- initAppMem
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
              logText $ "Got SnakeDirection from " <> client
            _ -> logText $ "Unexpected command from " <> client
          handleInputCommands appMem
        handleGameState :: AppMem -> IO ()
        handleGameState appMem = do
          runStep appMem
          status <- getStatus appMem
          speedFactor <- getSpeedFactor appMem
          when (status == GAMEOVER) $ do resetAppMem appMem
          world <- getWorld appMem
          logText $ "Sending tick to client " <> client
          WS.sendTextData conn $ encode $ Tick world
          threadDelay $ truncate $ initialTickDelay / speedFactor
          handleGameState appMem

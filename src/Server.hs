module Server where

import Control.Concurrent as C (modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Concurrent.Async (concurrently_)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Network.WebSockets as WS
import Relude
import Say (say)
import Snake
  ( AppMem,
    Direction,
    WStatus (GAMEOVER),
    World,
    getSpeedFactor,
    getStatus,
    getWorld,
    initAppMem,
    resetAppMem,
    runStep,
    setDirection,
  )

data ProtoMessages
  = Hello Text
  | Welcome
  | SnakeDirection Direction
  | Tick World
  | Bye
  deriving (Show, Generic)

instance ToJSON ProtoMessages

instance FromJSON ProtoMessages

type ClientID = Text

type ServerState = [ClientID]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: ClientID -> ServerState -> Bool
clientExists = elem

addClient :: ClientID -> ServerState -> ServerState
addClient client clients = client : clients

logText :: Text -> IO ()
logText = say

getProtoMessage :: WS.Connection -> IO ProtoMessages
getProtoMessage conn = do
  jsonMsg <- WS.receiveData conn
  case decode jsonMsg of
    Just msg -> pure msg
    Nothing -> error "Protocol violation. Unabled to decode message."

main :: IO ()
main = do
  s <- C.newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application s

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
            WS.sendTextData conn $ encode Welcome
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

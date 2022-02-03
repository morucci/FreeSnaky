module Server where

import Control.Concurrent as C (modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Concurrent.Async (concurrently_)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Network.WebSockets (Connection)
import qualified Network.WebSockets as WS
import Relude
import Say (say)
import Snake (AppMem, Direction, World, getWorld, initAppMem, runStep)

data Client = Client
  { cIdent :: Text,
    cConn :: WS.Connection
  }

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists (Client ident _) st = ident `elem` (cIdent <$> st)

addClient :: Client -> ServerState -> ServerState
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
      Hello ident -> do
        let cIdent = ident
            cConn = conn
         in handleHello $ Client {..}
      _ -> logText "Protocol violation. Expected Hello."
    pure ()
  where
    handleHello :: Client -> IO ()
    handleHello client@Client {..} = do
      clients <- C.readMVar stM
      logText $ "Received HELLO ident: " <> cIdent
      if clientExists client clients
        then do
          logText "Client exists. Sending Bye."
          WS.sendTextData cConn $ encode Bye
        else do
          modifyMVar_ stM $ \st -> do
            logText "Client new. Sending Welcome."
            WS.sendTextData cConn $ encode Welcome
            pure $ addClient client st
          handle client

    handle :: Client -> IO ()
    handle Client {..} = do
      wStateM <- initAppMem
      concurrently_ (handleInputCommands cConn) (handleGameState wStateM)
      where
        handleInputCommands :: Connection -> IO ()
        handleInputCommands conn = run
          where
            run = do
              _ <- getProtoMessage conn
              logText $ "Got Input from " <> cIdent
              run
        handleGameState :: AppMem -> IO ()
        handleGameState wStateM = run
          where
            tickDelay = 500000
            run = do
              threadDelay tickDelay
              modifyMVar_ stM $ \st -> do
                runStep wStateM
                world <- getWorld wStateM
                logText $ "Sending tick to client " <> cIdent
                WS.sendTextData cConn $ encode $ Tick world
                pure st
              run

data ProtoMessages
  = Hello Text
  | Welcome
  | MoveSnake Direction
  | Tick World
  | Bye
  deriving (Show, Generic)

instance ToJSON ProtoMessages

instance FromJSON ProtoMessages
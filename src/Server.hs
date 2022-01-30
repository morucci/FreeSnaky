module Server where

import Control.Concurrent as C (modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Data.Aeson (ToJSON)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Relude
import Snake (Direction, World)

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  putStrLn $ toString message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

main :: IO ()
main = do
  s <- C.newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application s

application :: MVar ServerState -> WS.ServerApp
application st pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    msg <- WS.receiveData conn
    clients <- C.readMVar st
    case msg of
      _
        | not (prefix `T.isPrefixOf` msg) ->
          WS.sendTextData conn ("Wrong announcement" :: Text)
        | clientExists client clients ->
          WS.sendTextData conn ("User already exists" :: Text)
        | otherwise -> flip finally disconnect $ do
          modifyMVar_ st $ \s -> do
            let s' = addClient client s
            WS.sendTextData conn $
              "Welcome! User: "
                <> T.intercalate ", " (map fst s)
            broadcast (fst client <> " joined") s'
            pure s'
          talk client st
        where
          prefix = "Hi! I am "
          client = (T.drop (T.length prefix) msg, conn)
          disconnect = do
            s <- modifyMVar st $ \s ->
              let s' = removeClient client s in return (s', s')
            broadcast (fst client <> " disconnected") s

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) s = forever $ do
  msg <- WS.receiveData conn
  C.readMVar s
    >>= broadcast
      (user `mappend` ": " `mappend` msg)

data ProtoMessages
  = Hello
  | StartGame
  | MoveSnake Direction
  | Tick World
  | EndGame
  | Bye
  deriving (Show, Generic)

instance ToJSON ProtoMessages

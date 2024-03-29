{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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
    WStatus (PAUSE),

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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Morph (hoist)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Streaming.Network.Internal (HostPreference (Host))
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime)
import Data.Time.Format (defaultTimeLocale)
import GHC.Generics (Generic)
import LeaderBoard
  ( Board (Board),
    BoardEntry (BoardEntry),
    LeaderBoard,
    loadLeaderBoard,
    readLeaderBoard,
    writeLeaderBoard,
  )
import Lucid
import Lucid.Base (commuteHtmlT)
import Lucid.XStatic (xstaticScripts)
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid
import Servant.XStatic (xstaticServant)
import Snake
import System.IO
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
import qualified XStatic as XStatic
import qualified XStatic.Tailwind as XStatic
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
  | -- | Ping message
    Ping UTCTime
  | -- | Pong message
    Pong UTCTime
  | -- | Pause game
    Pause
  | -- | Start a new game
    NewGame
  | -- | Stop a game
    EndGame
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
snakeWSApp :: TimedFastLogger -> ServerState -> WS.PendingConnection -> IO ()
snakeWSApp logger st pending = do
  conn <- WS.acceptRequest pending
  logMsg logger "Received incoming connection"
  WS.withPingThread conn 30 (pure ()) $ do
    msg <- getProtoMessage conn
    case msg of
      Hello ident -> handleWelcomeClient ident conn
      _ -> logMsg logger "Protocol violation. Expected Hello."
  where
    sendLeaderBoard :: WS.Connection -> IO ()
    sendLeaderBoard conn = do
      board <- readLeaderBoard $ leaderBoard st
      WS.sendBinaryData conn $ serialise $ LeaderBoard board

    handleWelcomeClient :: ClientID -> WS.Connection -> IO ()
    handleWelcomeClient client conn = do
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
          handleClient client conn

    handleClient :: ClientID -> WS.Connection -> IO ()
    handleClient client conn = do
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
            NewGame -> setRunning appMem
            EndGame -> do
              resetAppMem appMem
              WS.sendBinaryData conn $ serialise EndGame
            SnakeDirection dir -> do
              status <- gameStatus appMem
              when (status == RUNNING) $ setDirection appMem dir
            Ping date -> WS.sendBinaryData conn $ serialise $ Pong date
            Pause -> setPause appMem
            Bye -> do
              logMsg logger $ "Got Bye message from " <> client
              removeClient client st
              error "End" -- Force both threads to end
            _ -> logMsg logger $ "Unexpected command from " <> client
          handleInputCommands appMem
        handleGameState :: AppMem -> IO ()
        handleGameState appMem = do
          status <- gameStatus appMem
          case status of
            RUNNING -> do
              (world, newStatus, speedFactor, score) <- runStep appMem
              WS.sendBinaryData conn $ serialise $ Tick world
              when (newStatus == GAMEOVER) $ do
                addScore logger (leaderBoard st) client score
                handleGameState appMem
              let minDelay = 200000
                  delay = max minDelay $ truncate $ initialTickDelay / speedFactor
              threadDelay delay
              handleGameState appMem
            PAUSE -> do
              WS.sendBinaryData conn $ serialise Pause
              threadDelay 250000
              handleGameState appMem
            NEWGAME -> do
              threadDelay 250000
              handleGameState appMem
            GAMEOVER -> do
              sendLeaderBoard conn
              resetAppMem appMem
              WS.sendBinaryData conn $ serialise EndGame
              handleGameState appMem

-- Functions to start the server
--------------------------------

data NetworkAddr = NetworkAddr
  { nAddr :: String,
    nPort :: Int
  }
  deriving (Show)

type FreeSnakyWebAPIv1 =
  "xstatic" :> Raw
    :<|> "ws" :> "snaky" :> "cbor" :> WebSocketPending
    :<|> "status" :> Get '[HTML] (Html ())

freeSnakyWebAPIv1 :: Proxy FreeSnakyWebAPIv1
freeSnakyWebAPIv1 = Proxy

snakyCborServer :: TimedFastLogger -> ServerState -> Server WebSocketPending
snakyCborServer logger state = streamData
  where
    streamData :: MonadIO m => WS.PendingConnection -> m ()
    streamData pending = liftIO $ snakeWSApp logger state pending

freeSnakyServer :: TimedFastLogger -> ServerState -> Server FreeSnakyWebAPIv1
freeSnakyServer logger state =
  xstaticServant [XStatic.tailwind]
    :<|> snakyCborServer logger state
    :<|> commuteHtmlT (statusHtml state)

freeSnakyApp :: TimedFastLogger -> ServerState -> Wai.Application
freeSnakyApp logger state = serve freeSnakyWebAPIv1 $ freeSnakyServer logger state

-- | Run a local server on port 9160
runServerLocal :: IO ()
runServerLocal = runServer (NetworkAddr "127.0.0.1" 9160) LogNone

runServer :: NetworkAddr -> LogType' LogStr -> IO ()
runServer NetworkAddr {..} logType = do
  serverState <- newServerState
  case serverState of
    Left err -> putStrLn $ "$ Unable to initialize FreeSnaky due to: " <> from err
    Right st -> do
      timeCache <- newTimeCache simpleTimeFormat
      (logger, _) <- newTimedFastLogger timeCache logType
      let warpS = Warp.setPort nPort $ Warp.setHost (Host nAddr) $ Warp.defaultSettings
      Warp.runSettings warpS $ freeSnakyApp logger st

-- To be used by hoist to generalize the inner Mondad
generalize :: (Monad m) => Identity a -> m a
generalize m = return (runIdentity m)

gXstaticScripts :: MonadIO m => [XStatic.XStaticFile] -> HtmlT m ()
gXstaticScripts xsf = hoist generalize $ xstaticScripts xsf

leaderBoardHtml :: MonadIO m => LeaderBoard -> HtmlT m ()
leaderBoardHtml lb = do
  board <- liftIO $ readLeaderBoard lb
  let Board entries = board
  div_ [class_ "container mx-auto px-10"] $ do
    div_ [class_ "border-4 border-green-400 rounded font-mono"] $ do
      div_ [class_ "flex justify-center bg-green-400"] $ do
        h2_ [class_ "font-semibold text-xl"] "LeaderBoard"
      table_ [class_ "border-separate border-spacing-1 bg-green-100 w-full"] $ do
        thead_ $ do
          tr_ [class_ "text-left"] $ do
            th_ "Date"
            th_ "Name"
            th_ "Score"
        tbody_ $ do
          mapM_ renderRow entries
  where
    renderRow :: Monad m => BoardEntry -> HtmlT m ()
    renderRow (BoardEntry ident score date) = tr_ $ do
      td_ $ toHtml $ formatD date
      td_ $ toHtml ident
      td_ $ toHtml $ show score
    formatD :: UTCTime -> String
    formatD d = formatTime defaultTimeLocale "%F %T" d

statusHtml :: MonadIO m => ServerState -> HtmlT m ()
statusHtml ServerState {leaderBoard} = do
  doctypehtml_ $ do
    head_ $ do
      title_ "Status page"
      gXstaticScripts [XStatic.tailwind]
    body_ $ do
      leaderBoardHtml leaderBoard

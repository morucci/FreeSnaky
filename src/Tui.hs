-- |
-- Module      : Tui
-- Description : A terminal UI WebSocket client for the Snake game
-- Copyright   : (c) Fabien Boucher, 2022
-- License     : MIT
-- Maintainer  : fabien.dot.boucher@gmail.com
--
-- This module a terminal UI WebSocket client for the Snake game
module Tui
  ( -- * Function to start the TUI client
    runClient,
    runClientLocal,
  )
where

import Brick
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Control.Concurrent.Async (withAsync)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Server as S
  ( Direction (..),
    Item (..),
    ProtoMessage (..),
    World (..),
    getProtoMessage,
  )
import Witch
import Prelude

-- Various types for the Brick Engine
-------------------------------------

newtype Tick = Tick S.World

data Name = MainView deriving (Eq, Ord, Show)

-- | A data type that keep track of the application State
data SnakeAppState = SnakeAppState
  { appWState :: Maybe S.World,
    _appConn :: WS.Connection
  }

-- | Draw the UI according to the 'SnakeAppState'
drawUI :: SnakeAppState -> [Widget Name]
drawUI (SnakeAppState Nothing _conn) = [vBox [str "Waiting for server map"]]
drawUI (SnakeAppState (Just S.World {..}) _conn) =
  [ withBorderStyle BS.unicodeBold $
      B.borderWithLabel (str "Free Snaky") gameView
  ]
  where
    gameView = vBox [headerWidget, snakeWorldWidget]
    headerWidget = str $ "Score: " <> show wScore
    snakeWorldWidget = vBox rows
    rows = [hBox $ cellsInRow r | r <- [0 .. wHeight -1]]
    cellsInRow y = [drawCoord (x, y) | x <- [0 .. wWidth -1]]
    drawCoord (x, y) = case (wFlattenedMap !! x) !! y of
      S.SB -> withAttr snakeAttr $ str "o"
      S.BL -> withAttr blockAttr $ str " "
      S.Void -> str " "
      S.FD -> withAttr foodAttr $ str "*"
      S.EFD -> withAttr snakeAttr $ str "O"
      S.COLLISION -> withAttr collisionAttr $ str "x"

-- | Handle application events
handleEvent :: SnakeAppState -> BrickEvent Name Tick -> EventM Name (Next SnakeAppState)
handleEvent s@(SnakeAppState _ conn) event = case event of
  VtyEvent (V.EvKey V.KEsc []) -> do
    liftIO $ WS.sendClose conn $ encode S.Bye
    void . liftIO $ S.getProtoMessage conn
    halt s
  AppEvent (Tick newWorld) -> continue $ s {appWState = Just newWorld}
  VtyEvent (V.EvKey V.KRight []) -> handleDirEvent S.RIGHT
  VtyEvent (V.EvKey V.KLeft []) -> handleDirEvent S.LEFT
  VtyEvent (V.EvKey V.KUp []) -> handleDirEvent S.UP
  VtyEvent (V.EvKey V.KDown []) -> handleDirEvent S.DOWN
  _ -> continue s
  where
    handleDirEvent :: S.Direction -> EventM Name (Next SnakeAppState)
    handleDirEvent dir = do
      liftIO $ WS.sendTextData conn $ encode (S.SnakeDirection dir)
      continue s

foodAttr, blockAttr, snakeAttr, collisionAttr :: AttrName
foodAttr = "foodAttr"
blockAttr = "blockAttr"
snakeAttr = "snakeAttr"
collisionAttr = "collisionAttr"

-- | The Brick application definition
brickApp :: App SnakeAppState Tick Name
brickApp =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }
  where
    theMap :: AttrMap
    theMap =
      attrMap
        V.defAttr
        [ (collisionAttr, fg V.brightRed),
          (foodAttr, fg V.yellow),
          (blockAttr, bg V.white),
          (snakeAttr, fg V.green)
        ]

-- | Client App to run once the WS is connected
runClientApp :: T.Text -> WS.ClientApp ()
runClientApp clientId conn = do
  WS.sendTextData conn $ encode (S.Hello clientId)
  resp <- S.getProtoMessage conn
  case resp of
    S.Hello _ -> do
      chan <- newBChan 10
      let initialState = SnakeAppState Nothing conn
          buildVty = V.mkVty V.defaultConfig
      initialVty <- buildVty
      withAsync (readServerMessages chan) $ \_ -> do
        void $ customMain initialVty buildVty (Just chan) brickApp initialState
    S.Bye -> putStrLn "Client already connected"
    _ -> putStrLn "Unhandled server message"
  where
    readServerMessages :: BChan Tick -> IO ()
    readServerMessages chan = do
      resp <- S.getProtoMessage conn
      case resp of
        S.Tick world -> do
          writeBChan chan $ Tick world
          readServerMessages chan
        S.Bye -> do
          -- TODO: update SnakeAppState to warn UI about server disconnection
          putStrLn "Server closed connection"
        _ -> do
          -- TODO: update SnakeAppState to warn UI about server disconnection
          putStrLn "Unhandled server message"

-- Main functions
-----------------

-- | Run a TUI Client
runClient :: T.Text -> Int -> T.Text -> IO ()
runClient addr port ident =
  withSocketsDo $
    WS.runClient (from addr) port "/" $ runClientApp ident

-- | Run a TUI Client by connection on the local server
runClientLocal :: T.Text -> IO ()
runClientLocal = runClient "127.0.0.1" 9160

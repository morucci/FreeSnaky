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
import Codec.Serialise (serialise)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, sort)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified Graphics.Vty as V
import LeaderBoard as L
  ( Board (Board),
    BoardEntry (BoardEntry),
  )
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Server as S
  ( Direction (..),
    Item (..),
    NetworkAddr (..),
    ProtoMessage (..),
    World (..),
    getProtoMessage,
  )
import Text.Printf (printf)
import Witch
import Prelude

-- Various types for the Brick Engine
-------------------------------------

-- | A data type that define server events where UI must react
data ServerEvent
  = Tick S.World
  | LeaderBoard L.Board
  | ServerPing NominalDiffTime
  | QuitGame
  | Pause

data Name = MainView deriving (Eq, Ord, Show)

-- | A data type that keep track of the application State
data SnakeAppState = SnakeAppState
  { appWState :: Maybe S.World,
    appLeaderBoard :: Maybe L.Board,
    appServerPing :: Maybe NominalDiffTime,
    appPause :: Bool,
    _appConn :: WS.Connection
  }

-- | Draw the UI according to the 'SnakeAppState'
drawUI :: SnakeAppState -> [Widget Name]
drawUI (SnakeAppState worldM boardM serverPingM pause _) =
  [ withBorderStyle BS.unicodeBold $
      B.borderWithLabel (str "FreeSnaky") mainWidget
  ]
  where
    mainWidget =
      hBox
        [ hLimit 50 $ padRight Max $ vBox [infoWidget, snakeWorldWidget],
          vBox [str " "],
          hLimit 25 $ padRight Max $ vBox [str "Leader board", leaderBoardWidget]
        ]
    infoWidget = hBox $ intercalate [str ""] [scoreWidget, pingWidget, pauseWidget]
      where
        pingWidget = maybe mempty (\v -> [str $ "Ping: " <> printf "%.5s ms" (show v)]) serverPingM
        scoreWidget = maybe mempty (\w -> [str $ "Score: " <> show (S.wScore w), str " "]) worldM
        pauseWidget = case worldM of
          Just _ -> if pause then [str " 'p' unpause"] else [str " 'p' pause"]
          Nothing -> mempty
    snakeWorldWidget = case worldM of
      Nothing ->
        vBox
          [ str " ",
            str "Welcome in FreeSnaky world !",
            str " ",
            str "Press 's' to start the game",
            str "Press 'q' to quit a running game",
            str "Press 'Esc' to quit FreeSnaky"
          ]
      (Just (S.World {..})) -> vBox rows
        where
          rows = [hBox $ cellsInRow r | r <- [0 .. wHeight - 1]]
          cellsInRow y = [drawCoord (x, y) | x <- [0 .. wWidth - 1]]
          drawCoord (x, y) = case wFlattenedMap !! x !! y of
            S.SB -> withAttr snakeAttr $ str "o"
            S.BL -> withAttr blockAttr $ str " "
            S.Void -> str " "
            S.FD -> withAttr foodAttr $ str "*"
            S.EFD -> withAttr snakeAttr $ str "O"
            S.COLLISION -> withAttr collisionAttr $ str "x"
    leaderBoardWidget = case boardM of
      Just (Board board) -> vBox $ map mkEntry (take 25 $ sort board)
      _ -> str ""
      where
        mkEntry (L.BoardEntry name score _date) =
          let maxLen = 25
              scoreLen = length $ show score
              nameLen = T.length name
              dotLen = let rem' = maxLen - scoreLen - nameLen in if rem' <= 0 then 0 else rem'
              paddingDot = from $ replicate dotLen '.'
           in str $ from (name <> paddingDot <> from (show score))

-- | Handle application events
handleEvent :: SnakeAppState -> BrickEvent Name ServerEvent -> EventM Name (Next SnakeAppState)
handleEvent s@(SnakeAppState _ _board _serverPing _pause conn) event = case event of
  VtyEvent (V.EvKey V.KEsc []) -> do
    liftIO . WS.sendClose conn $ serialise S.Bye
    void . liftIO $ S.getProtoMessage conn
    halt s
  AppEvent (Tick newWorld) ->
    continue $
      s
        { appWState = Just newWorld,
          appPause = False
        }
  AppEvent (LeaderBoard newLBoard) -> continue $ s {appLeaderBoard = Just newLBoard}
  AppEvent (ServerPing dt) -> continue $ s {appServerPing = Just dt}
  AppEvent Pause -> continue $ s {appPause = True}
  AppEvent QuitGame ->
    continue $
      s
        { appWState = Nothing,
          appPause = False
        }
  VtyEvent (V.EvKey V.KRight []) -> handleDirEvent S.RIGHT
  VtyEvent (V.EvKey V.KLeft []) -> handleDirEvent S.LEFT
  VtyEvent (V.EvKey V.KUp []) -> handleDirEvent S.UP
  VtyEvent (V.EvKey V.KDown []) -> handleDirEvent S.DOWN
  VtyEvent (V.EvKey (V.KChar 'p') []) -> do
    liftIO $ WS.sendBinaryData conn $ serialise S.Pause
    continue s
  VtyEvent (V.EvKey (V.KChar 's') []) -> do
    liftIO $ WS.sendBinaryData conn $ serialise S.NewGame
    continue s
  VtyEvent (V.EvKey (V.KChar 'q') []) -> do
    liftIO $ WS.sendBinaryData conn $ serialise S.EndGame
    continue s
  _ -> continue s
  where
    handleDirEvent :: S.Direction -> EventM Name (Next SnakeAppState)
    handleDirEvent dir = do
      liftIO $ WS.sendBinaryData conn $ serialise (S.SnakeDirection dir)
      continue s

foodAttr, blockAttr, snakeAttr, collisionAttr :: AttrName
foodAttr = "foodAttr"
blockAttr = "blockAttr"
snakeAttr = "snakeAttr"
collisionAttr = "collisionAttr"

-- | The Brick application definition
brickApp :: App SnakeAppState ServerEvent Name
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
  WS.sendBinaryData conn . serialise $ S.Hello clientId
  resp <- S.getProtoMessage conn
  case resp of
    S.Hello _ -> do
      chan <- newBChan 10
      withAsync (readServerMessages chan) $ \_ ->
        withAsync sendPingMessages $ \_ ->
          runBrickApp chan
    S.Bye -> putStrLn "Client already connected"
    _ -> putStrLn "Unhandled server message"
  where
    runBrickApp :: BChan ServerEvent -> IO ()
    runBrickApp chan = do
      let initialState = SnakeAppState Nothing Nothing Nothing False conn
          buildVty = V.mkVty V.defaultConfig
      initialVty <- buildVty
      void $ customMain initialVty buildVty (Just chan) brickApp initialState
    sendPingMessages :: IO ()
    sendPingMessages = do
      now <- getCurrentTime
      liftIO $ WS.sendBinaryData conn $ serialise (S.Ping now)
      threadDelay 5000000
      sendPingMessages
    readServerMessages :: BChan ServerEvent -> IO ()
    readServerMessages chan = do
      resp <- S.getProtoMessage conn
      case resp of
        S.Tick world -> do
          writeBChan chan $ Tick world
          readServerMessages chan
        S.LeaderBoard board -> do
          writeBChan chan $ LeaderBoard board
          readServerMessages chan
        S.Pong sentDate -> do
          now <- getCurrentTime
          writeBChan chan . ServerPing $ diffUTCTime now sentDate
          readServerMessages chan
        S.Pause -> do
          writeBChan chan Pause
          readServerMessages chan
        S.EndGame -> do
          writeBChan chan QuitGame
          readServerMessages chan
        S.Bye -> putStrLn "Server closed connection"
        _ -> putStrLn "Unhandled server message"

-- Main functions
-----------------

-- | Run a TUI Client
runClient :: S.NetworkAddr -> T.Text -> IO ()
runClient S.NetworkAddr {..} ident =
  withSocketsDo $
    WS.runClient nAddr nPort "/ws/snaky/cbor" $
      runClientApp ident

-- | Run a TUI Client by connection on the local server
runClientLocal :: T.Text -> IO ()
runClientLocal = runClient $ S.NetworkAddr "127.0.0.1" 9160

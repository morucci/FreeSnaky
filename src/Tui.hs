module Tui where

import Brick
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Control.Concurrent.Async (withAsync)
import Data.Aeson (encode)
import qualified Graphics.Vty as V
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Relude
import qualified Server as S
  ( ProtoMessages (Bye, Hello, SnakeDirection, Tick, Welcome),
    getProtoMessage,
  )
import Snake
  ( Direction (..),
    Item (..),
    World (..),
  )

newtype Tick = Tick World

data Name = MainView deriving (Eq, Ord, Show)

data AppState = AppState
  { appWState :: Maybe World,
    appConn :: WS.Connection
  }

drawUI :: AppState -> [Widget Name]
drawUI (AppState Nothing _conn) = [vBox [str "Waiting for server map"]]
drawUI (AppState (Just World {..}) _conn) =
  [ withBorderStyle BS.unicodeBold $
      B.borderWithLabel (str "Free Snaky") $ vBox rows
  ]
  where
    rows = [hBox $ cellsInRow r | r <- [0 .. height -1]]
    cellsInRow y = [drawCoord (x, y) | x <- [0 .. width -1]]
    drawCoord (x, y) = case m !!? x of
      Just r -> case r !!? y of
        Just SB -> str "o"
        Just BL -> str "#"
        Just Void -> str " "
        Just FD -> str "F"
        Just COLLISION -> str "X"
        Nothing -> str " Out of bounds"
      Nothing -> error "Out of bounds"
    height = wHeight
    width = wWidth
    m = wFlattenedMap

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent s@(AppState _ conn) event = case event of
  VtyEvent (V.EvKey V.KEsc []) -> halt s
  AppEvent (Tick newWorld) -> continue $ s {appWState = Just newWorld}
  VtyEvent (V.EvKey V.KRight []) -> handleDirEvent RIGHT
  VtyEvent (V.EvKey V.KLeft []) -> handleDirEvent LEFT
  VtyEvent (V.EvKey V.KUp []) -> handleDirEvent UP
  VtyEvent (V.EvKey V.KDown []) -> handleDirEvent DOWN
  _ -> continue s
  where
    handleDirEvent :: Snake.Direction -> EventM Name (Next AppState)
    handleDirEvent dir = do
      liftIO $ WS.sendTextData conn $ encode (S.SnakeDirection dir)
      continue s

theMap :: AttrMap
theMap = attrMap V.defAttr []

app :: App AppState Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" runApp
  where
    runApp :: WS.ClientApp ()
    runApp conn = do
      let clientId = "fakeid"
      WS.sendTextData conn $ encode (S.Hello clientId)
      resp <- S.getProtoMessage conn
      case resp of
        S.Welcome -> do
          chan <- newBChan 10
          let initialState = AppState Nothing conn
              buildVty = V.mkVty V.defaultConfig
          initialVty <- buildVty
          withAsync (readServerMessages chan) $ \_ -> do
            void $ customMain initialVty buildVty (Just chan) app initialState
        S.Bye -> do
          pure ()
        _ -> print ("Not Implemented" :: Text)
      where
        readServerMessages :: BChan Tick -> IO ()
        readServerMessages chan = do
          resp <- S.getProtoMessage conn
          case resp of
            S.Tick world -> do
              writeBChan chan $ Tick world
            _ -> pure ()
          readServerMessages chan

module Tui where

import Brick
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Control.Concurrent (forkIO, threadDelay)
import qualified Graphics.Vty as V
import Relude
import Snake
  ( AppMem,
    Direction (..),
    Item (..),
    WStatus (..),
    World (..),
    getWorld,
    initAppMem,
    resetAppMem,
    runStep,
    setDirection,
  )

newtype Tick = Tick World

data Name = MainView deriving (Eq, Ord, Show)

data AppState = AppState
  { appWState :: World,
    -- TODO: appMem must not be required after client/server segmentation
    appMem :: AppMem
  }

drawUI :: AppState -> [Widget Name]
drawUI s = [withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Free Snaky") $ vBox rows]
  where
    rows = [hBox $ cellsInRow r | r <- [0 .. height -1]]
    cellsInRow y = [drawCoord (x, y) | x <- [0 .. width -1]]
    drawCoord (x, y) = case m !!? x of
      Just r -> case r !!? y of
        Just SB -> str "o"
        Just BL -> str "#"
        Just Void -> str " "
        Just COLLISION -> str "X"
        Nothing -> str " Out of bounds"
      Nothing -> error "Out of bounds"
    height = wHeight $ appWState s
    width = wWidth $ appWState s
    m = wFlattenedMap $ appWState s

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s (AppEvent (Tick newWorld)) = continue $ s {appWState = newWorld}
handleEvent s (VtyEvent (V.EvKey V.KRight [])) = handleDirEvent s RIGHT
handleEvent s (VtyEvent (V.EvKey V.KLeft [])) = handleDirEvent s LEFT
handleEvent s (VtyEvent (V.EvKey V.KUp [])) = handleDirEvent s UP
handleEvent s (VtyEvent (V.EvKey V.KDown [])) = handleDirEvent s DOWN
handleEvent s _ = continue s

handleDirEvent :: AppState -> Snake.Direction -> EventM Name (Next AppState)
handleDirEvent s dir = do
  liftIO $ setDirection (appMem s) dir
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
main = do
  chan <- newBChan 10
  mem <- initAppMem
  initialWorld <- getWorld mem
  let initialState = AppState initialWorld mem
      buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void . forkIO . forever $ do
    runStep mem
    world <- getWorld mem
    writeBChan chan $ Tick world
    threadDelay 500000
    when (wStatus world == GAMEOVER) $ resetAppMem mem
  void $ customMain initialVty buildVty (Just chan) app initialState

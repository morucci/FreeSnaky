module Tui where

import Brick
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Control.Concurrent (forkIO, threadDelay)
import qualified Graphics.Vty as V
import Relude
import Snake
  ( Coord (Coord),
    Item (SB),
    SnakeBody (SnakeBody),
    WMap (mHeight, mWidth),
    mkMap,
    queryMap,
  )

data Tick = Tick

data Name = MainView deriving (Eq, Ord, Show)

newtype AppState = AppState {sMap :: WMap}

drawUI :: AppState -> [Widget Name]
drawUI s = [withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Free Snaky") $ vBox rows]
  where
    rows = [hBox $ cellsInRow r | r <- [height -1, height -2 .. 0]]
    cellsInRow y = [drawCoord (x, y) | x <- [0 .. width -1]]
    drawCoord (x, y) = case queryMap (sMap s) (Coord x y) of
      Just (SB _) -> str "o"
      _ -> str " "
    height = mHeight $ sMap s
    width = mWidth $ sMap s

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s _ = continue s

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
  let initialState = AppState mkMap
      buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void . forkIO . forever $ do
    writeBChan chan Tick
    threadDelay 100000
  void $ customMain initialVty buildVty (Just chan) app initialState

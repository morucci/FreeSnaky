module Tui where

import Brick
import Brick.BChan
import Control.Concurrent (forkIO, threadDelay)
import qualified Graphics.Vty as V
import Relude
import Snake (WMap, mkMap)

data Tick = Tick

data Name = MainView deriving (Eq, Ord, Show)

newtype AppState = AppState {sMap :: WMap}

drawUI :: AppState -> [Widget Name]
drawUI s = [viewport MainView Both $ str "Test"]

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

module Snake
  ( runStep,
    getWorld,
    initAppMem,
    resetAppMem,
    World (..),
    Item (..),
    Direction (..),
    setDirection,
    AppMem,
    WStatus (..),
  )
where

import Control.Concurrent.MVar
import Relude hiding (newEmptyMVar, newMVar, putMVar, readMVar)
import System.Random (randomRIO)

data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)

newtype SnakeBody = SnakeBody Coord deriving (Show)

newtype Block = Block Coord deriving (Show)

newtype Food = Food Coord deriving (Show)

data Item = SB | BL | FD | COLLISION | Void deriving (Show, Eq)

type Snaky = [SnakeBody]

data Direction = UP | DOWN | RIGHT | LEFT deriving (Show, Eq)

data MovingSnaky = MovingSnaky {direction :: Direction, snake :: Snaky} deriving (Show)

data WStatus = GAMEOVER | RUNNING deriving (Show, Eq)

newtype AppMem = AppMem (MVar WState)

data WState = WState
  { mSnake :: MovingSnaky,
    mWidth :: Int,
    mHeight :: Int,
    mBlocks :: [Block],
    mFood :: Food,
    mStatus :: WStatus,
    mSpeedFactor :: Float
  }
  deriving (Show)

-- World should be the only data passed to the client
data World = World
  { wHeight :: Int,
    wWidth :: Int,
    wStatus :: WStatus,
    wFlattenedMap :: [[Item]],
    wSpeedFactor :: Float
  }
  deriving (Show)

-- >>> mkSnake $ Coord 5 5
-- MovingSnaky {direction = UP, snake = [SnakeBody (Coord {x = 5, y = 5}),SnakeBody (Coord {x = 5, y = 4})]}
mkSnake :: Coord -> MovingSnaky
mkSnake (Coord ix iy) = MovingSnaky UP $ foldr func [] $ mkSnake' 3
  where
    func :: Coord -> Snaky -> Snaky
    func (Coord _ _) acc = acc <> [SnakeBody $ Coord ix (iy - length acc)]
    mkSnake' = flip replicate (Coord 0 0)

-- >>> moveSnake . mkSnake $ Coord 5 5
-- MovingSnaky {direction = UP, snake = [SnakeBody (Coord {x = 5, y = 6}),SnakeBody (Coord {x = 5, y = 5}),SnakeBody (Coord {x = 5, y = 4})]}
moveSnake :: MovingSnaky -> MovingSnaky
moveSnake MovingSnaky {..} =
  MovingSnaky direction $ case direction of
    UP -> move snake shiftU
    DOWN -> move snake shiftD
    RIGHT -> move snake shiftR
    LEFT -> move snake shiftL
  where
    move :: Snaky -> (SnakeBody -> SnakeBody) -> Snaky
    move s sfunc = case s of
      [] -> error "Invalid Snaky"
      x : xs -> let head' = sfunc x in [head'] <> shiftBody x xs

    shiftBody :: SnakeBody -> Snaky -> Snaky
    shiftBody c s = case s of
      [] -> s
      x : xs -> [c] <> shiftBody x xs

    shiftU (SnakeBody (Coord x y)) = SnakeBody $ Coord x (y + 1)
    shiftD (SnakeBody (Coord x y)) = SnakeBody $ Coord x (y - 1)
    shiftR (SnakeBody (Coord x y)) = SnakeBody $ Coord (x + 1) y
    shiftL (SnakeBody (Coord x y)) = SnakeBody $ Coord (x - 1) y

-- >>> moveAndIncreaseSnake . mkSnake $ Coord 5 5
-- MovingSnaky {direction = UP, snake = [SnakeBody (Coord {x = 5, y = 6}),SnakeBody (Coord {x = 5, y = 5}),SnakeBody (Coord {x = 5, y = 4}),SnakeBody (Coord {x = 5, y = 3})]}
moveAndIncreaseSnake :: MovingSnaky -> MovingSnaky
moveAndIncreaseSnake ms@(MovingSnaky _ s) =
  let tail' = case reverse s of
        [] -> error "Invalid Snaky"
        co : _ -> co
      newMS = moveSnake ms
   in newMS {snake = snake newMS <> [tail']}

-- >>> setSnakeDirection RIGHT $ mkSnake $ Coord 5 5
-- MovingSnaky {direction = RIGHT, snake = [SnakeBody (Coord {x = 5, y = 5}),SnakeBody (Coord {x = 5, y = 4})]}
setSnakeDirection :: Direction -> MovingSnaky -> MovingSnaky
setSnakeDirection newDir ms@(MovingSnaky dir s)
  | newDir == dir = ms
  | newDir == UP && dir == DOWN = ms
  | newDir == DOWN && dir == UP = ms
  | newDir == LEFT && dir == RIGHT = ms
  | newDir == RIGHT && dir == LEFT = ms
  | otherwise = MovingSnaky newDir s

-- >>> getSnakeCoord . mkSnake $ Coord 5 5
-- Coord {x = 5, y = 5}
getSnakeCoord :: MovingSnaky -> Coord
getSnakeCoord (MovingSnaky _ s) = case s of
  SnakeBody co : _ -> co
  [] -> error "Invalid Snaky"

stateToWorld :: WState -> World
stateToWorld wm =
  let wStatus = mStatus wm
      flattened = reverse $ foldr buildRow [] [0 .. mWidth wm - 1]
      wFlattenedMap = flattened
      wHeight = mHeight wm
      wWidth = mWidth wm
      wSpeedFactor = mSpeedFactor wm
   in World {..}
  where
    buildRow x acc = acc <> [foldr (buildCol x) [] [0 .. mHeight wm - 1]]
    buildCol :: Int -> Int -> [Item] -> [Item]
    buildCol x y acc' = acc' <> [getItem wm (Coord x y)]

getItem :: WState -> Coord -> Item
getItem wm coord =
  case (isSnakeBody, isBlock, isFood) of
    (Just sb, Nothing, Nothing) -> sb
    (Nothing, Just blk, Nothing) -> blk
    (Just _, Just _, Nothing) -> COLLISION
    (Nothing, Nothing, Just fd) -> fd
    _ -> Void
  where
    isSnakeBody =
      case filter (\(SnakeBody c) -> c == coord) $ snake $ mSnake wm of
        [] -> Nothing
        _ -> Just SB
    isBlock =
      case filter (\(Block c) -> c == coord) $ mBlocks wm of
        [] -> Nothing
        _ -> Just BL
    isFood =
      let (Food c) = mFood wm
       in if c == coord then Just FD else Nothing

getRandomCoord :: Int -> Int -> IO Coord
getRandomCoord width height = do
  x <- randomRIO (minWidth, maxWidth)
  y <- randomRIO (minHeight, maxHeight)
  pure $ Coord x y
  where
    maxWidth = width - 2
    maxHeight = height - 2
    minWidth = 1
    minHeight = 1

mkMap :: IO WState
mkMap = do
  food <- mkFood width height
  pure $ WState (mkSnake $ Coord 25 12) width height mkBounds food RUNNING 1.0
  where
    mkBounds :: [Block]
    mkBounds =
      [Block $ Coord 0 y | y <- [0 .. height]]
        <> [Block $ Coord (width -1) y | y <- [0 .. height]]
        <> [Block $ Coord x 0 | x <- [0 .. width]]
        <> [Block $ Coord x (height -1) | x <- [0 .. width]]
    width = 50
    height = 25

mkFood :: Int -> Int -> IO Food
mkFood width height = do
  coord <- getRandomCoord width height
  pure $ Food coord

initAppMem :: IO AppMem
initAppMem = do
  m' <- mkMap
  m <- newMVar m'
  pure $ AppMem m

resetAppMem :: AppMem -> IO ()
resetAppMem (AppMem mem) = do
  modifyMVar_ mem doM
  where
    doM :: WState -> IO WState
    doM _ = mkMap

runStep :: AppMem -> IO ()
runStep (AppMem mem) = do
  modifyMVar_ mem doM
  where
    doM :: WState -> IO WState
    doM s = do
      let newSnake = moveSnake $ mSnake s
          newSnakeCoord = getSnakeCoord newSnake
      case getItem s newSnakeCoord of
        BL -> pure $ s {mSnake = newSnake, mStatus = GAMEOVER}
        FD -> do
          newFood <- mkFood (mWidth s) (mHeight s)
          pure $
            s
              { mSnake = moveAndIncreaseSnake $ mSnake s,
                mFood = newFood,
                mSpeedFactor = mSpeedFactor s * 1.1
              }
        _otherwise -> pure $ s {mSnake = newSnake}

setDirection :: AppMem -> Direction -> IO ()
setDirection (AppMem mem) dir = do
  modifyMVar_ mem doM
  where
    doM :: WState -> IO WState
    doM s =
      pure $ s {mSnake = setSnakeDirection dir $ mSnake s}

getWorld :: AppMem -> IO World
getWorld (AppMem mem) = do
  s <- readMVar mem
  pure $ stateToWorld s

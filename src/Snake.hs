-- |
-- Module      : Snake
-- Description : The Snake game engine
-- Copyright   : (c) Fabien Boucher, 2022
-- License     : MIT
-- Maintainer  : fabien.dot.boucher@gmail.com
--
-- This module contains data types and function to manage the
-- the Snake game engine
module Snake
  ( -- * Types
    Direction (..),
    World (..),
    Item (..),
    WStatus (..),
    AppMem,

    -- * Functions to change the game state
    initAppMem,
    resetAppMem,
    runStep,
    setDirection,

    -- * Functions to get game states
    getStatus,
    getSpeedFactor,
    getWorld,
  )
where

import Control.Concurrent.MVar
import Data.Aeson (FromJSON, ToJSON)
import Relude hiding (newEmptyMVar, newMVar, putMVar, readMVar)
import System.Random (randomRIO)

-- Internal Game state data
---------------------------

-- | A simple data type to describe coordinate
data Coord = Coord
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq)

-- | A Snake body element
newtype SnakeBody = SnakeBody Coord deriving (Show)

-- | A Block element
newtype Block = Block Coord deriving (Show)

-- | A Food element
newtype Food = Food Coord deriving (Show)

-- | A type alias that represent a Snake
type Snaky = [SnakeBody]

-- | A Snake with a moving direction
data MovingSnaky = MovingSnaky
  { -- | The Snake direction
    direction :: Direction,
    -- | The Snake
    snake :: Snaky
  }
  deriving (Show)

-- | The internal Game state
data WState = WState
  { -- | The Snake with its direction (Moving Snake)
    mSnake :: MovingSnaky,
    -- | The map width
    mWidth :: Int,
    -- | The map height
    mHeight :: Int,
    -- | The map blocks
    mBlocks :: [Block],
    -- | The current Food
    mFood :: Food,
    -- | The game status
    mStatus :: WStatus,
    -- | The current game speed factor
    mSpeedFactor :: Float
  }
  deriving (Show)

-- | A newtype which contains a Mvar of WState
newtype AppMem = AppMem (MVar WState)

-- API data to interface with a client
--------------------------------------

-- | World map Items
data Item
  = -- | A Snake Body Item
    SB
  | -- | A Block Item
    BL
  | -- | A Food Item
    FD
  | -- | A Collision Item
    COLLISION
  | -- | An empty Item
    Void
  deriving (Show, Eq, Generic)

instance ToJSON Item

instance FromJSON Item

-- | Snake direction
data Direction
  = -- | Direction UP
    UP
  | -- | Direction DOWN
    DOWN
  | -- | Direction RIGHT
    RIGHT
  | -- | Direction LEFT
    LEFT
  deriving (Show, Eq, Generic)

instance ToJSON Direction

instance FromJSON Direction

-- | Game status
data WStatus
  = -- | A Game that is over
    GAMEOVER
  | -- | A game that is running
    RUNNING
  deriving (Show, Eq, Generic)

instance ToJSON WStatus

instance FromJSON WStatus

-- | Snake External Game state
data World = World
  { -- | The height of the game map
    wHeight :: Int,
    -- | The width of the game map
    wWidth :: Int,
    -- | The gate status
    wStatus :: WStatus,
    -- | The flattened representation of the game map
    wFlattenedMap :: [[Item]]
  }
  deriving (Show, Generic)

instance ToJSON World

instance FromJSON World

-- Pure functions
-----------------

-- | Create a Snake at a given coordinate
-- >>> mkSnake $ Coord 5 5
-- MovingSnaky {direction = UP, snake = [SnakeBody (Coord {x = 5, y = 5}),SnakeBody (Coord {x = 5, y = 4}),SnakeBody (Coord {x = 5, y = 3})]}
mkSnake :: Coord -> MovingSnaky
mkSnake (Coord ix iy) = MovingSnaky UP $ foldr func [] $ mkSnake' 3
  where
    func :: Coord -> Snaky -> Snaky
    func (Coord _ _) acc = acc <> [SnakeBody $ Coord ix (iy - length acc)]
    mkSnake' = flip replicate (Coord 0 0)

-- | Move a Snake according to its current direction
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

-- | Move a snake according to its current direction and increase its size
-- >>> moveAndIncreaseSnake . mkSnake $ Coord 5 5
-- MovingSnaky {direction = UP, snake = [SnakeBody (Coord {x = 5, y = 6}),SnakeBody (Coord {x = 5, y = 5}),SnakeBody (Coord {x = 5, y = 4}),SnakeBody (Coord {x = 5, y = 3})]}
moveAndIncreaseSnake :: MovingSnaky -> MovingSnaky
moveAndIncreaseSnake ms@(MovingSnaky _ s) =
  let tail' = case reverse s of
        [] -> error "Invalid Snaky"
        co : _ -> co
      newMS = moveSnake ms
   in newMS {snake = snake newMS <> [tail']}

-- | Set snake direction
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

-- | Get snake head coordinate
-- >>> getSnakeCoord . mkSnake $ Coord 5 5
-- Coord {x = 5, y = 5}
getSnakeCoord :: MovingSnaky -> Coord
getSnakeCoord (MovingSnaky _ s) = case s of
  SnakeBody co : _ -> co
  [] -> error "Invalid Snaky"

-- | Transform the internal state to external state (World)
stateToWorld :: WState -> World
stateToWorld wm =
  let wStatus = mStatus wm
      flattened = reverse $ foldr buildRow [] [0 .. mWidth wm - 1]
      wFlattenedMap = flattened
      wHeight = mHeight wm
      wWidth = mWidth wm
   in World {..}
  where
    buildRow x acc = acc <> [foldr (buildCol x) [] [0 .. mHeight wm - 1]]
    buildCol :: Int -> Int -> [Item] -> [Item]
    buildCol x y acc' = acc' <> [getItem wm (Coord x y)]

-- | Get an Item at a given coordinate
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

-- Non pure IO functions
------------------------

-- | Compute a random coordinate
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

-- | Instance an internal Game state
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

-- | Create food given map size
mkFood :: Int -> Int -> IO Food
mkFood width height = do
  coord <- getRandomCoord width height
  pure $ Food coord

-- | Instance the internal Game state mem
initAppMem :: IO AppMem
initAppMem = do
  m' <- mkMap
  m <- newMVar m'
  pure $ AppMem m

-- | Get the game status
getStatus :: AppMem -> IO WStatus
getStatus (AppMem mem) = do
  wStateM <- readMVar mem
  pure $ mStatus wStateM

-- | Get the game speed factor
getSpeedFactor :: AppMem -> IO Float
getSpeedFactor (AppMem mem) = do
  wStateM <- readMVar mem
  pure $ mSpeedFactor wStateM

-- | Reset the game
resetAppMem :: AppMem -> IO ()
resetAppMem (AppMem mem) = do
  modifyMVar_ mem doM
  where
    doM :: WState -> IO WState
    doM _ = mkMap

-- | Perform a Game tick
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

-- | Set Snake direction
setDirection :: AppMem -> Direction -> IO ()
setDirection (AppMem mem) dir = do
  modifyMVar_ mem doM
  where
    doM :: WState -> IO WState
    doM s =
      pure $ s {mSnake = setSnakeDirection dir $ mSnake s}

-- | Get the external Game state (World)
getWorld :: AppMem -> IO World
getWorld (AppMem mem) = do
  s <- readMVar mem
  pure $ stateToWorld s

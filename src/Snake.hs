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

data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)

newtype SnakeBody = SnakeBody Coord deriving (Show)

newtype Block = Block Coord deriving (Show)

data Item = SB | BL | COLLISION | Void deriving (Show, Eq)

type Snaky = [SnakeBody]

data Direction = UP | DOWN | RIGHT | LEFT deriving (Show, Eq)

data MovingSnaky = MovingSnaky {direction :: Direction, snake :: Snaky} deriving (Show)

data WStatus = GAMEOVER | RUNNING deriving (Show, Eq)

newtype AppMem = AppMem (MVar WState)

data WState = WState
  { mSnake :: MovingSnaky,
    mWidth :: Int,
    mHeight :: Int,
    mBlocks :: [Block]
  }
  deriving (Show)

-- World should be the only data passed to the client
data World = World
  { wHeight :: Int,
    wWidth :: Int,
    wStatus :: WStatus,
    wFlattenedMap :: [[Item]]
  }
  deriving (Show)

-- >>> mkSnake $ Coord 5 5
-- MovingSnaky {direction = UP, snake = [SnakeBody (Coord {x = 5, y = 5}),SnakeBody (Coord {x = 5, y = 4})]}
mkSnake :: Coord -> MovingSnaky
mkSnake (Coord ix iy) = MovingSnaky UP $ foldr func [] $ mkSnake 3
  where
    func :: Coord -> Snaky -> Snaky
    func (Coord _ _) acc = acc <> [SnakeBody $ Coord ix (iy - length acc)]
    mkSnake length = replicate length (Coord 0 0)

-- >>> moveSnake . mkSnake $ Coord 5 5
-- MovingSnaky {direction = UP, snake = [SnakeBody (Coord {x = 5, y = 6}),SnakeBody (Coord {x = 5, y = 5}),SnakeBody (Coord {x = 5, y = 4})]}
moveSnake :: MovingSnaky -> MovingSnaky
moveSnake ms@MovingSnaky {..} =
  MovingSnaky direction $ case direction of
    UP -> move snake shiftU
    DOWN -> move snake shiftD
    RIGHT -> move snake shiftR
    LEFT -> move snake shiftL
  where
    move :: Snaky -> (SnakeBody -> SnakeBody) -> Snaky
    move s sfunc = case s of
      [] -> error "Invalid Snaky"
      co : cos -> let head = sfunc co in [head] <> shiftBody co cos

    shiftBody :: SnakeBody -> Snaky -> Snaky
    shiftBody c s = case s of
      [] -> s
      co : cos -> [c] <> shiftBody co cos

    shiftU (SnakeBody (Coord x y)) = SnakeBody $ Coord x (y + 1)
    shiftD (SnakeBody (Coord x y)) = SnakeBody $ Coord x (y - 1)
    shiftR (SnakeBody (Coord x y)) = SnakeBody $ Coord (x + 1) y
    shiftL (SnakeBody (Coord x y)) = SnakeBody $ Coord (x - 1) y

-- >>> moveAndIncreaseSnake . mkSnake $ Coord 5 5
-- MovingSnaky {direction = UP, snake = [SnakeBody (Coord {x = 5, y = 6}),SnakeBody (Coord {x = 5, y = 5}),SnakeBody (Coord {x = 5, y = 4}),SnakeBody (Coord {x = 5, y = 3})]}
moveAndIncreaseSnake :: MovingSnaky -> MovingSnaky
moveAndIncreaseSnake ms@(MovingSnaky dir s) =
  let tail = case reverse s of
        [] -> error "Invalid Snaky"
        co : _ -> co
      newMS = moveSnake ms
   in newMS {snake = snake newMS <> [tail]}

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

-- >>> getSnakeLength . mkSnake $ Coord 5 5
-- 2
getSnakeLength :: MovingSnaky -> Int
getSnakeLength (MovingSnaky _ s) = length s

stateToWorld :: WState -> World
stateToWorld wm =
  let wStatus = getStatus $ concat flattened
      flattened = reverse $ foldr buildRow [] [0 .. mWidth wm - 1]
      wFlattenedMap = flattened
      wHeight = mHeight wm
      wWidth = mWidth wm
   in World {..}
  where
    getStatus :: [Item] -> WStatus
    getStatus items = if COLLISION `elem` items then GAMEOVER else RUNNING
    buildRow x acc = acc <> [foldr (buildCol x) [] [0 .. mHeight wm - 1]]
    buildCol :: Int -> Int -> [Item] -> [Item]
    buildCol x y acc' = acc' <> [getItem wm (Coord x y)]
    getItem :: WState -> Coord -> Item
    getItem wm coord =
      case (isSnakeBody, isBlock) of
        (Just sb, Nothing) -> sb
        (Nothing, Just blk) -> blk
        (Nothing, Nothing) -> Void
        (Just _, Just _) -> COLLISION
      where
        isSnakeBody =
          case filter (\(SnakeBody c) -> c == coord) $ snake $ mSnake wm of
            [] -> Nothing
            _ -> Just SB
        isBlock =
          case filter (\(Block c) -> c == coord) $ mBlocks wm of
            [] -> Nothing
            _ -> Just BL

mkMap :: WState
mkMap = WState (mkSnake $ Coord 25 12) width height mkBounds
  where
    mkBounds :: [Block]
    mkBounds =
      [Block $ Coord 0 y | y <- [0 .. height]]
        <> [Block $ Coord (width -1) y | y <- [0 .. height]]
        <> [Block $ Coord x 0 | x <- [0 .. width]]
        <> [Block $ Coord x (height -1) | x <- [0 .. width]]
    width = 50
    height = 25

initAppMem :: IO AppMem
initAppMem = do
  m <- newMVar mkMap
  pure $ AppMem m

resetAppMem :: AppMem -> IO ()
resetAppMem (AppMem mem) = do
  modifyMVar_ mem modify
  where
    modify :: WState -> IO WState
    modify _ = pure mkMap

runStep :: AppMem -> IO ()
runStep (AppMem mem) = do
  modifyMVar_ mem modify
  where
    modify :: WState -> IO WState
    modify s =
      let newSnake = moveSnake $ mSnake s
       in pure $ s {mSnake = newSnake}

setDirection :: AppMem -> Direction -> IO ()
setDirection (AppMem mem) dir = do
  modifyMVar_ mem modify
  where
    modify :: WState -> IO WState
    modify s =
      pure $ s {mSnake = setSnakeDirection dir $ mSnake s}

getWorld :: AppMem -> IO World
getWorld (AppMem mem) = do
  s <- readMVar mem
  pure $ stateToWorld s

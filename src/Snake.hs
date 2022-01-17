module Snake
  ( moveSnake,
    moveAndIncreaseSnake,
    setSnakeDirection,
    getSnakeCoord,
    getSnakeLength,
    mkMap,
    runStep,
    WMap (mHeight, mWidth, mSnake),
    getMap,
    SnakeBody (SnakeBody),
    Item (SB, BL),
    Coord (Coord),
    Direction (RIGHT, LEFT, UP, DOWN),
  )
where

import Relude

data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)

newtype SnakeBody = SnakeBody Coord deriving (Show)

newtype Block = Block Coord deriving (Show)

data Item = SB SnakeBody | BL Block deriving (Show)

type Snaky = [SnakeBody]

data Direction = UP | DOWN | RIGHT | LEFT deriving (Show, Eq)

data MovingSnaky = MovingSnaky {direction :: Direction, snake :: Snaky} deriving (Show)

-- >>> mkSnake $ Coord 5 5
-- MovingSnaky {direction = UP, snake = [SnakeBody (Coord {x = 5, y = 5}),SnakeBody (Coord {x = 5, y = 4})]}
mkSnake :: Coord -> MovingSnaky
mkSnake (Coord ix iy) = MovingSnaky UP $ foldr func [] $ mkSnake 3
  where
    func :: Coord -> Snaky -> Snaky
    func (Coord _ _) acc = acc <> [SnakeBody $ Coord ix (iy - length acc)]
    mkSnake length = replicate length (Coord 0 0)

-- >>> moveSnake . mkSnake $ Coord 5 5
-- MovingSnaky {direction = UP, snake = [SnakeBody (Coord {x = 5, y = 6}),SnakeBody (Coord {x = 5, y = 5})]}
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
-- MovingSnaky {direction = UP, snake = [SnakeBody (Coord {x = 5, y = 6}),SnakeBody (Coord {x = 5, y = 5}),SnakeBody (Coord {x = 5, y = 4})]}
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

data WMap = WMap
  { mSnake :: MovingSnaky,
    mWidth :: Int,
    mHeight :: Int,
    mBlocks :: [Block]
  }
  deriving (Show)

getMap :: WMap -> [[Maybe Item]]
getMap wm = reverse $ foldr buildRow [] [0 .. mWidth wm - 1]
  where
    buildRow x acc = acc <> [foldr (buildCol x) [] [0 .. mHeight wm - 1]]
    buildCol :: Int -> Int -> [Maybe Item] -> [Maybe Item]
    buildCol x y acc' = acc' <> [getItem wm (Coord x y)]
    getItem :: WMap -> Coord -> Maybe Item
    getItem wm coord = isSnakeBody <|> isBlock
      where
        isSnakeBody =
          case filter (\(SnakeBody c) -> c == coord) $ snake $ mSnake wm of
            [] -> Nothing
            x : _ -> Just $ SB x
        isBlock =
          case filter (\(Block c) -> c == coord) $ mBlocks wm of
            [] -> Nothing
            x : _ -> Just $ BL x

mkMap :: WMap
mkMap = WMap (mkSnake $ Coord 25 12) width height mkBounds
  where
    mkBounds :: [Block]
    mkBounds =
      [Block $ Coord 0 y | y <- [0 .. height]]
        <> [Block $ Coord (width -1) y | y <- [0 .. height]]
        <> [Block $ Coord x 0 | x <- [0 .. width]]
        <> [Block $ Coord x (height -1) | x <- [0 .. width]]
    width = 50
    height = 25

runStep :: WMap -> WMap
runStep m = m {mSnake = moveSnake $ mSnake m}

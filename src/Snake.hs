module Snake
  ( mkSnake,
    moveSnake,
    moveAndIncreaseSnake,
    setSnakeDirection,
    getSnakeCoord,
    getSnakeLength,
  )
where

import Relude

data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)

type Snaky = [Coord]

data Direction = UP | DOWN | RIGHT | LEFT deriving (Show, Eq)

data MovingSnaky = MovingSnaky {direction :: Direction, snake :: Snaky} deriving (Show)

mkSnake :: Coord -> MovingSnaky
mkSnake (Coord ix iy) = MovingSnaky UP $ foldr func [] $ mkSnake 2
  where
    func :: Coord -> Snaky -> Snaky
    func (Coord _ _) acc = acc <> [Coord ix (iy - length acc)]
    mkSnake length = replicate length (Coord 0 0)

moveSnake :: MovingSnaky -> MovingSnaky
moveSnake ms@MovingSnaky {..} =
  MovingSnaky direction $ case direction of
    UP -> move snake shiftU
    DOWN -> move snake shiftD
    RIGHT -> move snake shiftR
    LEFT -> move snake shiftL
  where
    move :: Snaky -> (Coord -> Coord) -> Snaky
    move s sfunc = case s of
      [] -> error "Invalid Snaky"
      co : cos -> let head = sfunc co in [head] <> shiftBody co cos

    shiftBody :: Coord -> Snaky -> Snaky
    shiftBody c s = case s of
      [] -> s
      co : cos -> [c] <> shiftBody co cos

    shiftU (Coord x y) = Coord x (y + 1)
    shiftD (Coord x y) = Coord x (y - 1)
    shiftR (Coord x y) = Coord (x + 1) y
    shiftL (Coord x y) = Coord (x - 1) y

moveAndIncreaseSnake :: MovingSnaky -> MovingSnaky
moveAndIncreaseSnake ms@(MovingSnaky dir s) =
  let tail = case reverse s of
        [] -> error "Invalid Snaky"
        co : _ -> co
      newMS = moveSnake ms
   in newMS {snake = snake newMS <> [tail]}

setSnakeDirection :: Direction -> MovingSnaky -> MovingSnaky
setSnakeDirection newDir ms@(MovingSnaky dir s)
  | newDir == dir = ms
  | newDir == UP && dir == DOWN = ms
  | newDir == DOWN && dir == UP = ms
  | newDir == LEFT && dir == RIGHT = ms
  | newDir == RIGHT && dir == LEFT = ms
  | otherwise = MovingSnaky newDir s

getSnakeCoord :: MovingSnaky -> Coord
getSnakeCoord (MovingSnaky _ s) = case s of
  [] -> error "Invalid Snaky"
  co : _ -> co

getSnakeLength :: MovingSnaky -> Int
getSnakeLength (MovingSnaky _ s) = length s

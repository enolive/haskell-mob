{-# LANGUAGE RecordWildCards #-}

module MarsRover
  ( mkRover
  , commands
  , Rover
  , Direction(..)
  , direction
  , position
  , forward
  , backward
  , right
  , left
  , command
  ) where

data Rover = Rover
  { position  :: Position
  , direction :: Direction
  } deriving (Show, Eq)

type Position = (Int, Int)

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Enum, Bounded)

mkRover :: Rover
mkRover = Rover {position = (0, 0), direction = North}

commands :: Rover -> String -> Rover
commands = foldl (flip command)

forward :: Rover -> Rover
forward rover@Rover {..} = rover {position = newPosition position direction}
  where
    newPosition (x, y) North = (x, y + 1)
    newPosition (x, y) East  = (x + 1, y)
    newPosition (x, y) South = (x, y - 1)
    newPosition (x, y) West  = (x - 1, y)

right :: Rover -> Rover
right rover@Rover {..} = rover {direction = newDirection direction}
  where
    newDirection currentDirection
      | currentDirection == maxBound = minBound
      | otherwise = succ currentDirection

left :: Rover -> Rover
left = right . right . right

backward :: Rover -> Rover
backward = right . right . forward . right . right

command :: Char -> Rover -> Rover
command 'f' = forward
command 'b' = backward
command 'r' = right
command 'l' = left
command _ = undefined


{-# LANGUAGE RecordWildCards #-}

module MarsRover
  ( mkRover
  , commands
  , forward
  , backward
  , right
  , left
  , Rover(..)
  , Orientation(..)
  ) where

import           Control.Monad
import           Data.Maybe

data Orientation
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Enum, Bounded)

type Position = (Int, Int)

data Rover = Rover
  { orientation :: Orientation
  , position    :: Position
  } deriving (Show, Eq)

mkRover :: Rover
mkRover = Rover {orientation = North, position = (0, 0)}

commands :: Rover -> String -> Rover
commands rover = fromMaybe rover . foldM (flip maybeCommand) rover

maybeCommand :: Char -> Rover -> Maybe Rover
maybeCommand 'f' = Just . forward
maybeCommand 'b' = Just . backward
maybeCommand 'l' = Just . left
maybeCommand 'r' = Just . right
maybeCommand _   = const Nothing

forward :: Rover -> Rover
forward rover@Rover {..} = rover {position = newPosition position orientation}
  where
    newPosition (x, y) North = (x, y + 1)
    newPosition (x, y) South = (x, y - 1)
    newPosition (x, y) East  = (x + 1, y)
    newPosition (x, y) West  = (x - 1, y)

right :: Rover -> Rover
right rover@Rover {..} = rover {orientation = newOrientation}
  where
    newOrientation
      | orientation == maxBound = minBound
      | otherwise = succ orientation

left :: Rover -> Rover
left = right . right . right

backward :: Rover -> Rover
backward = right . right . forward . right . right

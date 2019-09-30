--{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module MarsRover
  ( Orientation(..)
  , Rover(position, orientation)
  , backward
  , commands
  , forward
  , mkRover
  , turnLeft
  , turnRight
  ) where

import           Control.Monad (foldM)
import           Data.Maybe    (fromMaybe)

class (Eq a, Enum a, Bounded a) =>
      CyclicEnum a
  where
  csucc :: a -> a
  csucc val
    | val == maxBound = minBound
    | otherwise = succ val

data Rover = Rover
  { position    :: Position
  , orientation :: Orientation
  } deriving (Eq, Show)

type Position = (Int, Int)

data Orientation
  = North
  | West
  | South
  | East
  deriving (Bounded, Enum, Eq, Show, CyclicEnum)

mkRover :: Rover
mkRover = Rover {position = (0, 0), orientation = North}

commands :: Rover -> String -> Rover
commands rover = fromMaybe rover . foldM (flip command) rover

command :: Char -> Rover -> Maybe Rover
command 'f' = Just . forward
command 'b' = Just . backward
command 'l' = Just . turnLeft
command 'r' = Just . turnRight
command _   = const Nothing

forward :: Rover -> Rover
forward rover@Rover {..} = rover {position = newPosition orientation position}
  where
    newPosition North (x, y) = (x, y + 1)
    newPosition West (x, y)  = (x + 1, y)
    newPosition South (x, y) = (x, y - 1)
    newPosition East (x, y)  = (x - 1, y)

turnLeft :: Rover -> Rover
turnLeft rover@Rover {..} = rover {orientation = csucc orientation}

turnRight :: Rover -> Rover
turnRight = turnLeft . turnLeft . turnLeft

backward :: Rover -> Rover
backward = turnLeft . turnLeft . forward . turnLeft . turnLeft

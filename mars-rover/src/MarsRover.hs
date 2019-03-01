module MarsRover
  ( mkRover
  , commands
  , Rover
  ) where

data Rover = Dummy deriving (Show)

mkRover :: Rover
mkRover = Dummy

commands :: Rover -> String -> Rover
commands rover xs = rover

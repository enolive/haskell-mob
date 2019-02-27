module MarsRover
  ( mkRover
  , commands
  , Rover
  ) where

import           Control.Monad (foldM)
import           Data.Maybe    (fromMaybe)

data Rover = Dummy deriving (Show)

mkRover :: Rover
mkRover = error "implement me!"

commands :: Rover -> String -> Rover
commands rover xs = error "implement me!"

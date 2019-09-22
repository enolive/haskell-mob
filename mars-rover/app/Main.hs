module Main where

import           MarsRover (Rover, commands, mkRover)

main :: IO ()
main = putStrLn "enter commands or exit with blank line" >> gameLoop mkRover

gameLoop :: Rover -> IO ()
gameLoop initRover = do
  print initRover
  line <- getLine
  if null line
    then putStrLn "bye!"
    else gameLoop $ commands initRover line

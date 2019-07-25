module Main where

import           MarsRover

main :: IO ()
main = putStrLn "enter commands or exit with blank line" >> gameLoop mkRover

gameLoop :: Rover -> IO ()
gameLoop initRover = print initRover >> getLine >>= loopOrExit initRover
  where
    loopOrExit _ []       = putStrLn "bye!"
    loopOrExit rover line = gameLoop $ commands rover line

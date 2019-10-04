module MarsRoverSpec
  ( spec
  ) where

import           MarsRover
import           Test.Hspec

spec :: Spec
spec =
  describe "Mars Rover" $ do
    let rover = mkRover
    context "initialization" $ do
      it "default orientation should be north" $ orientation rover `shouldBe` North
      it "default position should be 0,0" $ position rover `shouldBe` (0, 0)
    context "commands" $ do
      it "should offer a forward command that moves the rover into its orientation" $ do
        forward rover {position = (0, 0), orientation = North} `shouldBe` rover {position = (0, 1), orientation = North}
        forward rover {position = (5, 1), orientation = North} `shouldBe` rover {position = (5, 2), orientation = North}
        forward rover {position = (5, 1), orientation = East} `shouldBe` rover {position = (6, 1), orientation = East}
        forward rover {position = (-2, 3), orientation = West} `shouldBe` rover {position = (-3, 3), orientation = West}
        forward rover {position = (-3, 5), orientation = South} `shouldBe`
          rover {position = (-3, 4), orientation = South}
      it "should offer a right command that turns the rover clockwise" $ do
        right rover {orientation = North} `shouldBe` rover {orientation = East}
        right rover {orientation = East} `shouldBe` rover {orientation = South}
        right rover {orientation = South} `shouldBe` rover {orientation = West}
        right rover {orientation = West} `shouldBe` rover {orientation = North}
      it "should offer a left command that turns the rover counter-clockwise" $ do
        left rover {orientation = North} `shouldBe` rover {orientation = West}
        left rover {orientation = East} `shouldBe` rover {orientation = North}
        left rover {orientation = South} `shouldBe` rover {orientation = East}
        left rover {orientation = West} `shouldBe` rover {orientation = South}
      it "should offer a backward command that moves the rover against its orientation" $ do
        backward rover {position = (0, 0), orientation = North} `shouldBe`
          rover {position = (0, -1), orientation = North}
        backward rover {position = (5, 1), orientation = North} `shouldBe`
          rover {position = (5, 0), orientation = North}
        backward rover {position = (5, 1), orientation = East} `shouldBe` rover {position = (4, 1), orientation = East}
        backward rover {position = (-2, 3), orientation = West} `shouldBe`
          rover {position = (-1, 3), orientation = West}
        backward rover {position = (-3, 5), orientation = South} `shouldBe`
          rover {position = (-3, 6), orientation = South}
    context "single character as command" $ do
      it "should offer f as short for forward" $ commands rover "f" `shouldBe` forward rover
      it "should offer b as short for backward" $ commands rover "b" `shouldBe` backward rover
      it "should offer l as short for left" $ commands rover "l" `shouldBe` left rover
      it "should offer r as short for right" $ commands rover "r" `shouldBe` right rover
      it "should ignore all other characters" $ commands rover "q" `shouldBe` rover
    context "sequence of characters as commands" $ do
      it "should ignore an empty sequence" $ commands rover "" `shouldBe` rover
      it "should move forward twice on ff input" $ commands rover "ff" `shouldBe` rover{position = (0,2)}
      it "should move forward twice on frbf input" $ commands rover "frbf" `shouldBe` (forward . backward . right . forward) rover
    context "validation" $ do
      it "should ignore a command sequence with invalid characters" $ commands rover "xxx" `shouldBe` rover
      it "should ignore the whole command sequence" $ commands rover "fxx" `shouldBe` rover
    
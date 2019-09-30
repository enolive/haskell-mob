{-# OPTIONS_GHC -Wno-orphans #-}

module MarsRoverSpec
  ( spec
  ) where

import           MarsRover
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary Rover where
  arbitrary = mkRoverWith <$> arbitrary <*> arbitraryBoundedEnum
    where
      mkRoverWith p o = mkRover {position = p, orientation = o}

containsBothValidAndInvalidCommands :: String -> Bool
containsBothValidAndInvalidCommands = (&&) <$> any isValidCommand <*> any isInvalidCommand

isInvalidCommand :: Char -> Bool
isInvalidCommand = not . isValidCommand

isValidCommand :: Char -> Bool
isValidCommand = (`elem` "flbr")

spec :: Spec
spec =
  describe "Mars Rover" $ do
    context "initialization" $ do
      it "has a default position" $ position mkRover `shouldBe` (0, 0)
      it "has a default orientation" $ orientation mkRover `shouldBe` North
    context "single commands" $ do
      it "moves forward by one field according to its orientation" $ do
        forward mkRover `shouldBe` mkRover {position = (0, 1)}
        forward mkRover {position = (4, 2)} `shouldBe` mkRover {position = (4, 3)}
        forward mkRover {orientation = West} `shouldBe` mkRover {position = (1, 0), orientation = West}
        forward mkRover {orientation = South} `shouldBe` mkRover {position = (0, -1), orientation = South}
        forward mkRover {orientation = East} `shouldBe` mkRover {position = (-1, 0), orientation = East}
      it "turns left by 90 degrees" $ do
        turnLeft mkRover `shouldBe` mkRover {orientation = West}
        turnLeft mkRover {orientation = West} `shouldBe` mkRover {orientation = South}
        turnLeft mkRover {orientation = South} `shouldBe` mkRover {orientation = East}
        turnLeft mkRover {orientation = East} `shouldBe` mkRover {orientation = North}
      it "turns right by 90 degrees" $ property $ \r -> turnRight r `shouldBe` (turnLeft . turnLeft . turnLeft) r
      it "moves back by one field according to its orientation" $
        property $ \r -> backward r `shouldBe` (turnLeft . turnLeft . forward . turnLeft . turnLeft) r
    context "translating Char -> command" $ do
      it "moves forward on 'f'" $ property $ \rover -> commands rover "f" `shouldBe` forward rover
      it "moves backward on 'b'" $ property $ \rover -> commands rover "b" `shouldBe` backward rover
      it "turns left on 'l'" $ property $ \rover -> commands rover "l" `shouldBe` turnLeft rover
      it "turns right on 'r'" $ property $ \rover -> commands rover "r" `shouldBe` turnRight rover
    context "multiple commands" $ do
      it "moves forward twice" $ commands mkRover "ff" `shouldBe` mkRover {position = (0, 2)}
      it "executes multiple commands" $
        commands mkRover "fflllrrbbbbff" `shouldBe` mkRover {position = (-2, 2), orientation = West}
    context "ignoring errors in input" $ do
      it "ignores invalid single command" $
        property $ forAll (suchThat arbitrary isInvalidCommand) $ \char rover -> commands rover [char] `shouldBe` rover
      it "ignores invalid list of commands" $
        property $
        forAll (suchThat arbitrary containsBothValidAndInvalidCommands) $ \list rover ->
          commands rover list `shouldBe` rover

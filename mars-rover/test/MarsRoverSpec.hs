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
    let rover = mkRover
    context "initialization" $ do
      it "has a default position" $ position rover `shouldBe` (0, 0)
      it "has a default orientation" $ orientation rover `shouldBe` North
    context "single commands" $ do
      it "moves forward by one field according to its orientation" $ do
        forward rover `shouldBe` rover {position = (0, 1)}
        forward rover {position = (4, 2)} `shouldBe` rover {position = (4, 3)}
        forward rover {orientation = West} `shouldBe` rover {position = (1, 0), orientation = West}
        forward rover {orientation = South} `shouldBe` rover {position = (0, -1), orientation = South}
        forward rover {orientation = East} `shouldBe` rover {position = (-1, 0), orientation = East}
      it "turns left by 90 degrees" $ do
        turnLeft rover `shouldBe` rover {orientation = West}
        turnLeft rover {orientation = West} `shouldBe` rover {orientation = South}
        turnLeft rover {orientation = South} `shouldBe` rover {orientation = East}
        turnLeft rover {orientation = East} `shouldBe` rover {orientation = North}
      it "turns right by 90 degrees" $ property $ \r -> turnRight r `shouldBe` (turnLeft . turnLeft . turnLeft) r
      it "moves back by one field according to its orientation" $
        property $ \r -> backward r `shouldBe` (turnLeft . turnLeft . forward . turnLeft . turnLeft) r
    context "translating Char -> command" $ do
      it "moves forward on 'f'" $ property $ \r -> command 'f' r `shouldBe` forward <$> Just r
      it "moves backward on 'b'" $ property $ \r -> command 'b' r `shouldBe` backward <$> Just r
      it "turns left on 'l'" $ property $ \r -> command 'l' r `shouldBe` turnLeft <$> Just r
      it "turns right on 'r'" $ property $ \r -> command 'r' r `shouldBe` turnRight <$> Just r
    context "multiple commands" $ do
      it "moves forward twice" $ commands rover "ff" `shouldBe` rover {position = (0, 2)}
      it "executes multiple commands" $
        commands rover "fflllrrbbbbff" `shouldBe` rover {position = (-2, 2), orientation = West}
    context "ignoring errors in input" $ do
      it "ignores invalid single command" $
        property $ forAll (suchThat arbitrary isInvalidCommand) $ \c r -> command c r `shouldBe` Nothing
      it "ignores invalid list of commands" $
        property $ forAll (suchThat arbitrary containsBothValidAndInvalidCommands) $ \cs r -> commands r cs `shouldBe` r

module MarsRoverSpec
  ( spec
  ) where

import           MarsRover
import           Test.Hspec

spec :: Spec
spec = describe "Mars Rover" $
  context "something" $
    it "should work"
      $ True `shouldBe` False

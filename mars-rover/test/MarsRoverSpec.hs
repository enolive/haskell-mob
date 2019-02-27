module MarsRoverSpec
  ( spec
  ) where

import           MarsRover
import           Test.Hspec        (Spec, context, describe, it, shouldBe)
import           Test.Hspec.Runner (hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Mars Rover" $
  context "something" $
    it "should work" $
      True `shouldBe` False

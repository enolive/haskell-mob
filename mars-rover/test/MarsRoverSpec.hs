module MarsRoverSpec
  ( spec
  ) where

import           MarsRover
import           Test.Hspec

spec :: Spec
spec =
  describe "Mars Rover" $ do
    context "Initial Values for Rover" $ do
      it "Start position should be (0, 0)" $ position mkRover `shouldBe` (0, 0)
      it "Start direction should be North" $ direction mkRover `shouldBe` North
    context "Executing single commands" $ do
      it "should move forward" $ do
        forward mkRover `shouldBe` mkRover {position = (0, 1)}
        forward mkRover {position = (1, 2)} `shouldBe` mkRover {position = (1, 3)}
        forward mkRover {position = (1, 2), direction = East} `shouldBe` mkRover {position = (2, 2), direction = East}
        forward mkRover {position = (1, 2), direction = South} `shouldBe` mkRover {position = (1, 1), direction = South}
        forward mkRover {position = (1, 2), direction = West} `shouldBe` mkRover {position = (0, 2), direction = West}
      it "should turn right" $ do
        right mkRover {direction = North} `shouldBe` mkRover {direction = East}
        right mkRover {direction = East} `shouldBe` mkRover {direction = South}
        right mkRover {direction = South} `shouldBe` mkRover {direction = West}
        right mkRover {direction = West} `shouldBe` mkRover {direction = North}
      it "should turn left" $ do
        left mkRover {direction = North} `shouldBe` mkRover {direction = West}
        left mkRover {direction = West} `shouldBe` mkRover {direction = South}
        left mkRover {direction = South} `shouldBe` mkRover {direction = East}
        left mkRover {direction = East} `shouldBe` mkRover {direction = North}
      it "should move backward" $ do
        backward mkRover `shouldBe` mkRover {position = (0, -1)}
        backward mkRover {position = (1, 2)} `shouldBe` mkRover {position = (1, 1)}
        backward mkRover {position = (1, 2), direction = East} `shouldBe` mkRover {position = (0, 2), direction = East}
        backward mkRover {position = (1, 2), direction = South} `shouldBe`
          mkRover {position = (1, 3), direction = South}
        backward mkRover {position = (1, 2), direction = West} `shouldBe` mkRover {position = (2, 2), direction = West}
    context "Executing single commands" $ do
      it "should move forward on f" $ command 'f' mkRover `shouldBe` mkRover {position = (0, 1)}
      it "should move backward on b" $ command 'b' mkRover `shouldBe` mkRover {position = (0, -1)}
      it "should turn right on r" $ command 'r' mkRover `shouldBe` mkRover {direction = East}
      it "should turn left on l" $ command 'l' mkRover `shouldBe` mkRover {direction = West}
    context "Executing multiple commands" $ do
      it "should evaluate commands ffflllbrrf" $
        commands mkRover "ffflllbrrf" `shouldBe` mkRover {position = (-2 ,3), direction = West }

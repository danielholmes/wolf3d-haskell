module Wolf3D.WorldSpec (worldSpec) where

import Test.Hspec
import Wolf3D.Geom
import Wolf3D.World
import Data.Vector


worldSpec :: SpecWith ()
worldSpec =
  describe "Wolf3D.World" $
    describe "worldWallsTouching" $ do
      it "should return no walls if world has no walls" $
        let world = createWorld []
        in worldWallsTouching world (Rectangle (Vector2 0 0) (Vector2 10 10)) `shouldBe` []

      it "should return all walls if all within" $
        let
          wall = Wall (Vector2 0 0) (Vector2 10 0) Red
          world = createWorld [wall]
        in worldWallsTouching world (Rectangle (Vector2 0 0) (Vector2 10 10)) `shouldBe` [wall]

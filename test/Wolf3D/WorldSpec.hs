module Wolf3D.WorldSpec (worldSpec) where

import Test.Hspec
import Wolf3D.Geom
import Wolf3D.World
import Wolf3D.Types
import Data.Vector
import Data.Maybe


worldSpec :: SpecWith ()
worldSpec =
  describe "Wolf3D.World" $ do
    describe "worldWallsTouching" $ do
      it "should return no walls if world has no walls" $
        let world = createWorld []
        in worldWallsTouching world (Rectangle (Vector2 0 0) (Vector2 10 10)) `shouldBe` []

      it "should return all walls if all within" $
        let
          wall = Wall (Vector2 0 0) (Vector2 10 0) Red
          world = createWorld [wall]
        in worldWallsTouching world (Rectangle (Vector2 0 0) (Vector2 10 10)) `shouldBe` [wall]

    describe "castRayToClosestWall" $ do
      it "should return empty if no walls" $
        let
          world = createWorld []
          result = castRayToClosestWall world (createRay (Vector2 0 (-30)) (Vector2 0 30))
        in
          isNothing result `shouldBe` True

      it "should return empty if miss walls" $
        let
          world = createWorld [Wall (Vector2 (-100) (-100)) (Vector2 200 0) Red]
          result = castRayToClosestWall world (createRay (Vector2 0 (-30)) (Vector2 0 30))
        in
          isNothing result `shouldBe` True

      it "should return correct wall and position if hit" $
        let
          wall = Wall (Vector2 (-100) 100) (Vector2 200 0) Red
          world = createWorld [wall]
          result = castRayToClosestWall world (createRay (Vector2 0 (-30)) (Vector2 0 30))
        in
          result `shouldBe` Just (WallHit wall (Vector2 0 100) (posZDouble 130))

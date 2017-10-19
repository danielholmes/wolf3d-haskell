module Wolf3D.DisplaySpec (displaySpec) where

import Test.Hspec
import Data.Maybe
import Data.Vector
import Wolf3D.Display
import Wolf3D.Types
import Wolf3D.Geom
import Wolf3D.World


displaySpec :: SpecWith ()
displaySpec = do
  describe "Wolf3D.Display.rayLineIntersection" $ do
    it "should return nothing if collinear" $
      let
        result = rayLineIntersection (createRay (Vector2 0 0) (Vector2 0 1)) (Vector2 0 2, Vector2 0 1)
      in
        isNothing result `shouldBe` True

    it "should return nothing if parallel" $
      let
        result = rayLineIntersection (createRay (Vector2 0 0) (Vector2 0 1)) (Vector2 1 0, Vector2 0 1)
      in
        isNothing result `shouldBe` True

    it "should return result if line-line intersection" $
      let
        result = rayLineIntersection (createRay (Vector2 0 0) (Vector2 0 2)) (Vector2 (-1) 1, Vector2 2 0)
      in
        result `shouldBe` Just (Vector2 0 1)

    it "should return result if ray-line intersection" $
      let
        result = rayLineIntersection (createRay (Vector2 0 0) (Vector2 0 2)) (Vector2 (-1) 3, Vector2 2 0)
      in
        result `shouldBe` Just (Vector2 0 3)

    it "should return nothing if regular non intersect" $
      let
        result = rayLineIntersection (createRay (Vector2 0 0) (Vector2 0 2)) (Vector2 1 1, Vector2 1 0)
      in
        isNothing result `shouldBe` True

  describe "Wolf3D.Display.castRayToClosestWall" $ do
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
        result `shouldBe` Just (posZDouble 130, wall, Vector2 0 100)

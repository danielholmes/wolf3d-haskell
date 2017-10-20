module Wolf3D.DisplaySpec (displaySpec) where

import Test.Hspec
import Data.Maybe
import Data.Vector
import Wolf3D.Display
import Wolf3D.Geom


displaySpec :: SpecWith ()
displaySpec =
  describe "Wolf3D.Display" $
    describe "rayLineIntersection" $ do
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

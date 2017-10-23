module SimEngine.GeomSpec (geomSpec) where

import Test.Hspec
import Data.Vector
import Data.Maybe
import SimEngine.Geom
import Wolf3D.SpecHelp


geomSpec :: SpecWith ()
geomSpec =
  describe "SimEngine.Geom" $ do
    describe "moveRayAlongDirection" $ do
      it "should return correct for straight ray" $
        let
          ray = createRay (Vector2 0 0) (Vector2 0 1)
        in
          moveRayAlongDirection ray (-10) `shouldSatisfy` veryCloseToRay (createRay (Vector2 0 (-10)) (Vector2 0 1))

      it "should return correct for angled ray" $
        let
          ray = createRay (Vector2 10 10) (Vector2 1 1)
          newSize = 10 + sqrt 8
        in
          moveRayAlongDirection ray 4 `shouldSatisfy` veryCloseToRay (createRay (Vector2 newSize newSize) (Vector2 1 1))

    describe "rotateRay" $ do
      it "should return correct for no rotation" $
        let
          ray = createRay (Vector2 0 0) (Vector2 0 1)
        in
          rotateRay ray 0 `shouldSatisfy` veryCloseToRay ray

      it "should return correct for 90 degree" $
        let
          ray = createRay (Vector2 10 10) (Vector2 0 1)
        in
          rotateRay ray (pi / 2) `shouldSatisfy` veryCloseToRay (createRay (Vector2 10 10) (Vector2 1 0))

      it "should return correct for -45 degree" $
        let
          ray = createRay (Vector2 10 10) (Vector2 1 1)
        in
          rotateRay ray (-pi / 4) `shouldSatisfy` veryCloseToRay (createRay (Vector2 10 10) (Vector2 0 1))

    describe "angleToVector2" $ do
      it "should return correct for 0" $
        angleToVector2 0 `shouldSatisfy` veryCloseToVector2 (Vector2 0 1)

      it "should return correct for 180" $
        angleToVector2 pi `shouldSatisfy` veryCloseToVector2 (Vector2 0 (-1))

      it "should return correct for 90" $
        angleToVector2 (pi / 2) `shouldSatisfy` veryCloseToVector2 (Vector2 1 0)

      it "should return correct for -45" $
        angleToVector2 (-pi / 4) `shouldSatisfy` veryCloseToVector2 (vnormalise (Vector2 (-1) 1))

    describe "rectangleSides" $
      it "should return correct" $
        let
          rect = Rectangle (Vector2 0 0) (Vector2 10 10)
          top = (Vector2 0 0, Vector2 10 0)
          bottom = (Vector2 0 10, Vector2 10 0)
          left = (Vector2 0 0, Vector2 0 10)
          right = (Vector2 10 0, Vector2 0 10)
        in
          rectangleSides rect `shouldBe` [top, right, bottom, left]

    describe "rectangleTouchesLine" $ do
      it "should return true for start within" $
        let
          rect = Rectangle (Vector2 0 0) (Vector2 10 10)
          line = (Vector2 5 5, Vector2 10 0)
        in
          rectangleTouchesLine rect line `shouldBe` True

      it "should return false for totally outside" $
        let
          rect = Rectangle (Vector2 0 0) (Vector2 10 10)
          line = (Vector2 100 100, Vector2 10 0)
        in
          rectangleTouchesLine rect line `shouldBe` False

      it "should return true for end within" $
        let
          rect = Rectangle (Vector2 0 0) (Vector2 10 10)
          line = (Vector2 (-5) 5, Vector2 10 0)
        in
          rectangleTouchesLine rect line `shouldBe` True

      it "should return true for move right through" $
        let
          rect = Rectangle (Vector2 0 0) (Vector2 10 10)
          line = (Vector2 (-5) 5, Vector2 20 0)
        in
          rectangleTouchesLine rect line `shouldBe` True

      it "should return true for totally inside" $
        let
          rect = Rectangle (Vector2 100 100) (Vector2 10 10)
          line = (Vector2 105 105, Vector2 1 1)
        in
          rectangleTouchesLine rect line `shouldBe` True

      it "should return true for touching corner only" $
        let
          rect = Rectangle (Vector2 0 0) (Vector2 10 10)
          line = (Vector2 10 10, Vector2 5 5)
        in
          rectangleTouchesLine rect line `shouldBe` True

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

    describe "rectangleOverlapsRectangle" $ do
      it "should return false if outside" $
        let
          rect1 = Rectangle (Vector2 0 0) (Vector2 5 5)
          rect2 = Rectangle (Vector2 6 6) (Vector2 5 5)
        in
          rectangleOverlapsRectangle rect1 rect2 `shouldBe` False

      it "should return true if inside" $
        let
          rect1 = Rectangle (Vector2 0 0) (Vector2 5 5)
          rect2 = Rectangle (Vector2 1 1) (Vector2 1 1)
        in
          rectangleOverlapsRectangle rect1 rect2 `shouldBe` True

      it "should return true if surrounding" $
        let
          rect1 = Rectangle (Vector2 1 1) (Vector2 1 1)
          rect2 = Rectangle (Vector2 0 0) (Vector2 5 5)
        in
          rectangleOverlapsRectangle rect1 rect2 `shouldBe` True

      it "should return true if half overlap" $
        let
          rect1 = Rectangle (Vector2 0 0) (Vector2 10 10)
          rect2 = Rectangle (Vector2 5 5) (Vector2 10 10)
        in
          rectangleOverlapsRectangle rect1 rect2 `shouldBe` True

      it "should return false if just corners touch" $
        let
          rect1 = Rectangle (Vector2 0 0) (Vector2 10 10)
          rect2 = Rectangle (Vector2 10 10) (Vector2 10 10)
        in
          rectangleOverlapsRectangle rect1 rect2 `shouldBe` False

    describe "vector2ToAngle" $ do
      it "should return correct for 45 degrees" $
        vector2ToAngle (Vector2 4 4) `shouldBe` pi / 4

      it "should return correct for straight" $
        vector2ToAngle (Vector2 0 4) `shouldBe` 0

      it "should return correct for 60 degrees" $
        vector2ToAngle (Vector2 2 1) `shouldSatisfy` veryCloseToDouble 1.107148720000547

      it "should return correct for 30 degrees" $
        vector2ToAngle (Vector2 1 2) `shouldSatisfy` veryCloseToDouble 0.4636476067904533

module Wolf3D.GeomSpec (geomSpec) where

import Test.Hspec
import Data.Vector
import Wolf3D.Geom


geomSpec :: SpecWith ()
geomSpec =
  describe "Wolf3D.Geom" $ do
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

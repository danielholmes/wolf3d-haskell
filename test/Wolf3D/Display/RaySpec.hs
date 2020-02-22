module Wolf3D.Display.RaySpec (raySpec) where

import Test.Hspec
import Wolf3D.Display.Ray
import Wolf3D.Engine
import Data.Vector
import Data.List

halfTileGlobalSizeI :: Int
halfTileGlobalSizeI = tileGlobalSize `div` 2

halfTileGlobalSize :: Double
halfTileGlobalSize = fromIntegral halfTileGlobalSizeI

raySpec :: SpecWith ()
raySpec =
  describe "Wolf3D.Display.Ray" $ do
    describe "castRay" $ do
      it "should return hit for NE / 0-89" $
        let
          wm = transpose [[Nothing, Nothing, Just Red]
                        , [Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 halfTileGlobalSize (3.0 * fromIntegral tileGlobalSize)
          angle = 45
        in
          castRay wm pos angle `shouldBe` Just (Hit {material=Red
                                                    , direction=Horizontal
                                                    , intercept=(163839, tileGlobalSize)})

      -- NOTE:!! Check  this, not searching correctly, doing  2 hor in a row, no verts
      -- setup test to show error
      it "should return hit for NWish / 90-179" $
        let
          wm = transpose [[Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]
                        , [Just Red, Nothing, Nothing]]
          pos = Vector2 (2.5 * fromIntegral tileGlobalSize) (3.0 * fromIntegral tileGlobalSize)
          angle = 150
        in
          castRay wm pos angle `shouldBe` Just (Hit {material=Red
                                                    , direction=Vertical
                                                    , intercept=(tileGlobalSize, 139852)})

      it "should return hit for SW / 180-269" $
        let
          wm = transpose [[Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]
                        , [Just Red, Nothing, Nothing]]
          pos = Vector2 (3.0 * fromIntegral tileGlobalSize) halfTileGlobalSize
          angle = 225
        in
          castRay wm pos angle `shouldBe` Just (Hit {material=Red
                                                    , direction=Vertical
                                                    , intercept=(tileGlobalSize, 163839)})

      it "should return hit for SEish / 270-359" $
        let
          wm = transpose [[Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Just Red]]
          pos = Vector2 (1.5 * fromIntegral tileGlobalSize) 0
          angle = 300
        in
          castRay wm pos angle `shouldBe` Just (Hit {material=Red
                                                    , direction=Horizontal
                                                    , intercept=(173979, 131072)})

      it "should return hit for straight East / 0" $
        let
          wm = transpose [[Nothing, Nothing, Just Red]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 0 halfTileGlobalSize
          angle = 0
        in
          castRay wm pos angle `shouldBe` Just (Hit {material=Red
                                                    , direction=Vertical
                                                    , intercept=(2 * tileGlobalSize, halfTileGlobalSizeI)})

      it "should return hit for straight West / 180" $
        let
          wm = transpose [[Just Red, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 (2 * fromIntegral tileGlobalSize) halfTileGlobalSize
          angle = 180
        in
          castRay wm pos angle `shouldBe` Just (Hit {material=Red
                                                    , direction=Vertical
                                                    , intercept=(tileGlobalSize, halfTileGlobalSizeI)})

      it "should return hit for straight North / 90" $
        let
          wm = transpose [[Nothing, Just Red]
                        , [Nothing, Nothing]]
          pos = Vector2 (1.5 * fromIntegral tileGlobalSize) (2 * fromIntegral tileGlobalSize)
          angle = 90
        in
          castRay wm pos angle `shouldBe` Just (Hit {material=Red
                                                    , direction=Horizontal
                                                    , intercept=(tileGlobalSize + halfTileGlobalSizeI, tileGlobalSize)})

      it "should return hit for straight South / 270" $
        let
          wm = transpose [[Nothing, Nothing]
                        , [Nothing, Just Red]]
          pos = Vector2 (1.5 * fromIntegral tileGlobalSize) 0
          angle = 270
        in
          castRay wm pos angle `shouldBe` Just (Hit {material=Red
                                                    , direction=Horizontal
                                                    , intercept=(tileGlobalSize + halfTileGlobalSizeI, tileGlobalSize)})

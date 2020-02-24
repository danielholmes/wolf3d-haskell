module Wolf3D.Display.RaySpec (raySpec) where

import Test.Hspec
import Wolf3D.Display.Ray
import Wolf3D.Sim
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
    describe "castRayToWalls" $ do
      it "should return hit for NE / 0-89" $
        let
          wm = transpose [[Nothing, Nothing, Just Grey1]
                        , [Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 halfTileGlobalSize (3.0 * fromIntegral tileGlobalSize)
          angle = normalToFineAngle 45
          result = castRayToWalls wm pos angle angle
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=32767
                                        , distance=207635
                                        , direction=Horizontal
                                        , intercept=(163839, tileGlobalSize)}

      it "should return hit for NE / 0-89 expressed as minus" $
        let
          wm = transpose [[Nothing, Nothing, Just Grey1]
                        , [Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 halfTileGlobalSize (3.0 * fromIntegral tileGlobalSize)
          midAngle = normalToFineAngle 45
          angle = normalToFineAngle (45 - 360)
          result = castRayToWalls wm pos midAngle angle
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=32767
                                        , distance=207635
                                        , direction=Horizontal
                                        , intercept=(163839, tileGlobalSize)}

      it "should return hit for NE / 0-89 expressed as over 360" $
        let
          wm = transpose [[Nothing, Nothing, Just Grey1]
                        , [Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 halfTileGlobalSize (3.0 * fromIntegral tileGlobalSize)
          midAngle = normalToFineAngle 45
          angle = normalToFineAngle (45 + 360)
          result = castRayToWalls wm pos midAngle angle
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=32767
                                        , distance=207635
                                        , direction=Horizontal
                                        , intercept=(163839, tileGlobalSize)}

      it "should return hit for NWish / 90-179" $
        let
          wm = transpose [[Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]
                        , [Just Grey1, Nothing, Nothing]]
          pos = Vector2 (2.5 * fromIntegral tileGlobalSize) (3.0 * fromIntegral tileGlobalSize)
          angle = normalToFineAngle 150
          result = castRayToWalls wm pos angle angle
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=8780
                                        , distance=135784
                                        , direction=Vertical
                                        , intercept=(tileGlobalSize, 139852)}

      it "should return hit for SW / 180-269" $
        let
          wm = transpose [[Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]
                        , [Just Grey1, Nothing, Nothing]]
          pos = Vector2 (3.0 * fromIntegral tileGlobalSize) halfTileGlobalSize
          angle = normalToFineAngle 225
          result = castRayToWalls wm pos angle angle
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=32767
                                        , distance=207635
                                        , direction=Vertical
                                        , intercept=(tileGlobalSize, 163839)}

      it "should return hit for SEish / 270-359" $
        let
          wm = transpose [[Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Just Grey1]]
          pos = Vector2 (1.5 * fromIntegral tileGlobalSize) 0
          angle = normalToFineAngle 300
          result = castRayToWalls wm pos angle angle
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=42907
                                        , distance=173622
                                        , direction=Horizontal
                                        , intercept=(173979, 131072)}

      it "should return hit for straight East / 0" $
        let
          wm = transpose [[Nothing, Nothing, Just Grey1]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 0 halfTileGlobalSize
          angle = normalToFineAngle 0
          result = castRayToWalls wm pos angle angle
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=halfTileGlobalSizeI
                                        , distance=153344
                                        , direction=Vertical
                                        , intercept=(2 * tileGlobalSize, halfTileGlobalSizeI)}

      it "should return hit for straight West / 180" $
        let
          wm = transpose [[Just Grey1, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 (2 * fromIntegral tileGlobalSize) halfTileGlobalSize
          angle = normalToFineAngle 180
          result = castRayToWalls wm pos angle angle
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=halfTileGlobalSizeI
                                        , distance=87808
                                        , direction=Vertical
                                        , intercept=(tileGlobalSize, halfTileGlobalSizeI)}

      it "should return hit for straight North / 90" $
        let
          wm = transpose [[Nothing, Just Grey1]
                        , [Nothing, Nothing]]
          pos = Vector2 (1.5 * fromIntegral tileGlobalSize) (2 * fromIntegral tileGlobalSize)
          angle = normalToFineAngle 90
          result = castRayToWalls wm pos angle angle
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=halfTileGlobalSizeI
                                        , distance=87808
                                        , direction=Horizontal
                                        , intercept=(tileGlobalSize + halfTileGlobalSizeI, tileGlobalSize)}

      it "should return hit for straight South / 270" $
        let
          wm = transpose [[Nothing, Nothing]
                        , [Nothing, Just Grey1]]
          pos = Vector2 (1.5 * fromIntegral tileGlobalSize) 0
          angle = normalToFineAngle 270
          result = castRayToWalls wm pos angle angle
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=halfTileGlobalSizeI
                                        , distance=87808
                                        , direction=Horizontal
                                        , intercept=(tileGlobalSize + halfTileGlobalSizeI, tileGlobalSize)}

module Wolf3D.Display.RaySpec (raySpec) where

import Test.Hspec
import Wolf3D.Display.Data
import Wolf3D.Display.Ray
import Wolf3D.Sim
import Wolf3D.Engine
import Data.Vector
import Data.List

halfTileGlobalSizeI :: Int
halfTileGlobalSizeI = tileGlobalSize `div` 2

halfTileGlobalSize :: Double
halfTileGlobalSize = fromIntegral halfTileGlobalSizeI

dTileGlobalSize :: Double
dTileGlobalSize = fromIntegral tileGlobalSize

raySpec :: SpecWith ()
raySpec =
  describe "Wolf3D.Display.Ray" $ do
    describe "hitDistance" $ do
      it "should return correct for straight" $
        let
          focal = (0, 3 * tileGlobalSize)
          int = (0, 1 * tileGlobalSize)
          betaAngle = 0
          result = hitDistance betaAngle focal int
        in
          result `shouldBe` (2 * tileGlobalSize)

      it "should return correct for negative diagonal" $
        let
          focal = (0, 0)
          int = (2 * tileGlobalSize, 2 * tileGlobalSize)
          betaAngle = normalToFineAngle (-45)
          result = hitDistance betaAngle focal int
        in
          result `shouldBe` (2 * tileGlobalSize)

      it "should return correct for positive diagonal" $
        let
          focal = (0, 0)
          int = (2 * tileGlobalSize, 2 * tileGlobalSize)
          betaAngle = normalToFineAngle 45
          result = hitDistance betaAngle focal int
        in
          result `shouldBe` (2 * tileGlobalSize)

      it "should enforce min dist for 0" $
        let
          focal = (0, tileGlobalSize)
          int = (0, tileGlobalSize)
          betaAngle = 0
          result = hitDistance betaAngle focal int
        in
          result `shouldBe` 0x5800

      it "should enforce min dist non-0" $
        let
          focal = (0, tileGlobalSize)
          int = (0, tileGlobalSize)
          betaAngle = normalToFineAngle (-30)
          result = hitDistance betaAngle focal int
        in
          result `shouldBe` 0x5800

    describe "castRaysToWalls" $ do
      it "should return same, correct distance for straight wall" $
        let
          wm = transpose [replicate 20 (Just Grey1)
                          , [Just Grey1] ++ (replicate 18 Nothing) ++ [Just Grey1]
                          , [Just Grey1] ++ (replicate 18 Nothing) ++ [Just Grey1]
                          , [Just Grey1] ++ (replicate 18 Nothing) ++ [Just Grey1]
                          , [Just Grey1] ++ (replicate 18 Nothing) ++ [Just Grey1]
                          , [Just Grey1] ++ (replicate 18 Nothing) ++ [Just Grey1]
                          , replicate 20 (Just Grey1)]
          pos = Vector2 (10.0 * dTileGlobalSize) (3.0 * dTileGlobalSize - fromIntegral focalLength)
          angle = normalToFineAngle 90
          hits = castRaysToWalls wm pos angle
        in
          (all (\h -> (distance h == 2 * tileGlobalSize)) hits) `shouldBe` True

    describe "castRayToWalls" $ do
      it "should return hit for NE / 0-89" $
        let
          wm = transpose [[Nothing, Nothing, Just Grey1]
                        , [Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 halfTileGlobalSize (3.0 * fromIntegral tileGlobalSize)
          angle = normalToFineAngle 45
          result = castRayToWalls wm pos angle 0
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=32767
                                        , distance=293640
                                        , direction=Horizontal}

      it "should return hit for NE / 0-89 expressed as minus" $
        let
          wm = transpose [[Nothing, Nothing, Just Grey1]
                        , [Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 halfTileGlobalSize (3.0 * fromIntegral tileGlobalSize)
          midAngle = normalToFineAngle 45
          offset = normalToFineAngle (-90)
          result = castRayToWalls wm pos midAngle offset
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=32767
                                        , distance=293640
                                        , direction=Horizontal}

      it "should return hit for NWish / 90-179" $
        let
          wm = transpose [[Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]
                        , [Just Grey1, Nothing, Nothing]]
          pos = Vector2 (2.5 * fromIntegral tileGlobalSize) (3.0 * fromIntegral tileGlobalSize)
          angle = normalToFineAngle 150
          result = castRayToWalls wm pos angle 0
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=8780
                                        , distance=185484
                                        , direction=Vertical}

      it "should return hit for SW / 180-269" $
        let
          wm = transpose [[Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]
                        , [Just Grey1, Nothing, Nothing]]
          pos = Vector2 (3.0 * fromIntegral tileGlobalSize) halfTileGlobalSize
          angle = normalToFineAngle 225
          result = castRayToWalls wm pos angle 0
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=32767
                                        , distance=293640
                                        , direction=Vertical}

      it "should return hit for SEish / 270-359" $
        let
          wm = transpose [[Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Just Grey1]]
          pos = Vector2 (1.5 * fromIntegral tileGlobalSize) 0
          angle = normalToFineAngle 300
          result = castRayToWalls wm pos angle 0
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=42907
                                        , distance=237172
                                        , direction=Horizontal}

      it "should return hit for straight East / 0" $
        let
          wm = transpose [[Nothing, Nothing, Just Grey1]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 0 halfTileGlobalSize
          angle = normalToFineAngle 0
          result = castRayToWalls wm pos angle 0
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=halfTileGlobalSizeI
                                        , distance=focalLength + 2 * tileGlobalSize
                                        , direction=Vertical}

      it "should return hit for straight West / 180" $
        let
          wm = transpose [[Just Grey1, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 (2 * fromIntegral tileGlobalSize) halfTileGlobalSize
          angle = normalToFineAngle 180
          result = castRayToWalls wm pos angle 0
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=halfTileGlobalSizeI
                                        , distance=focalLength + tileGlobalSize
                                        , direction=Vertical}

      it "should return hit for straight North / 90" $
        let
          wm = transpose [[Nothing, Just Grey1]
                        , [Nothing, Nothing]]
          pos = Vector2 (1.5 * fromIntegral tileGlobalSize) (2 * fromIntegral tileGlobalSize)
          angle = normalToFineAngle 90
          result = castRayToWalls wm pos angle 0
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=halfTileGlobalSizeI
                                        , distance=focalLength + tileGlobalSize
                                        , direction=Horizontal}

      it "should return hit for straight South / 270" $
        let
          wm = transpose [[Nothing, Nothing]
                        , [Nothing, Just Grey1]]
          pos = Vector2 (1.5 * fromIntegral tileGlobalSize) 0
          angle = normalToFineAngle 270
          result = castRayToWalls wm pos angle 0
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=halfTileGlobalSizeI
                                        , distance=focalLength + tileGlobalSize
                                        , direction=Horizontal}

      it "should handle crash case correctly" $
        let
          wm = transpose [[Just Blue1, Just Blue1, Just Blue1, Just Blue1, Just Blue1]
                        , [Just Blue1, Nothing,    Nothing,    Nothing,    Just Blue1]
                        , [Just Blue1, Nothing,    Nothing,    Nothing,    Just Blue1]
                        , [Just Blue1, Nothing,    Nothing,    Nothing,    Just Grey1]
                        , [Just Blue1, Nothing,    Nothing,    Nothing,    Just Blue1]
                        , [Just Blue1, Just Blue1, Just Blue1, Just Blue1, Just Blue1]]
          pos = Vector2 (2.5 * dTileGlobalSize) (3.5 * dTileGlobalSize)
          angle = normalToFineAngle 43
          angleOffset = (-365)
          result = castRayToWalls wm pos angle angleOffset
        in
          result `shouldBe` WallRayHit {material=Grey1
                                        , tilePosition=21567
                                        , distance=97437
                                        , direction=Vertical}

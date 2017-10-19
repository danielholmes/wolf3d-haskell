module Wolf3D.GeomSpec (geomSpec) where

import Test.Hspec
import Data.Vector
import Wolf3D.Geom
import Wolf3D.SpecHelp


geomSpec :: SpecWith ()
geomSpec = do
  describe "Wolf3D.Geom.moveRayAlongDirection" $ do
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

  describe "Wolf3D.Geom.rotateRay" $ do
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

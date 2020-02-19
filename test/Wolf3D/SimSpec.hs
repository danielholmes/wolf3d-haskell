module Wolf3D.SimSpec (simSpec) where

import Test.Hspec
import Data.Vector
import Wolf3D.Geom
import Wolf3D.Sim
import Wolf3D.SpecHelp


vec2Unit45 :: Double
vec2Unit45 = sqrt 0.5

simSpec :: SpecWith ()
simSpec =
  describe "Wolf3D.Sim" $ do
    describe "heroLookRay" $ do
      it "should return correct length 1 for east" $
        let
          hero = createHero (Vector2 0 0)
        in
          heroLookRay hero `shouldSatisfy` veryCloseToRay (createRay (Vector2 0 0) (Vector2 1 0))

      it "should return correct for looking west" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (180 * 20) -- angleScale
        in
          heroLookRay hero `shouldSatisfy` veryCloseToRay (createRay (Vector2 0 0) (Vector2 (-1) 0))

      it "should return correct for looking 45 NE" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (45 * 20) -- angleScale
        in
          heroLookRay hero `shouldSatisfy` veryCloseToRay (createRay (Vector2 0 0) (Vector2 vec2Unit45 (-vec2Unit45)))

      it "should return correct for looking 45 SE" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (315 * 20) -- angleScale
        in
          heroLookRay hero `shouldSatisfy` veryCloseToRay (createRay (Vector2 0 0) (Vector2 vec2Unit45 vec2Unit45))

    describe "moveHero" $ do
      it "should move forward correctly if facing north" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (90 * 20) -- angleScale
        in
          position (moveHero hero 1) `shouldSatisfy` veryCloseToVector2 (Vector2 0 (-1))

      it "should move forward correctly if facing south" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (270 * 20) -- angleScale
        in
          position (moveHero hero 1) `shouldSatisfy` veryCloseToVector2 (Vector2 0 1)

      it "should move backward correctly if facing north" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (90 * 20) -- angleScale
        in
          position (moveHero hero (-1)) `shouldSatisfy` veryCloseToVector2 (Vector2 0 1)

      it "should move backward correctly if facing south" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (270 * 20) -- angleScale
        in
          position (moveHero hero (-1)) `shouldSatisfy` veryCloseToVector2 (Vector2 0 (-1))

      it "should move forward correctly if facing east" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (0 * 20) -- angleScale
        in
          position (moveHero hero 1) `shouldSatisfy` veryCloseToVector2 (Vector2 1 0)

      it "should move backward correctly if facing east" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (0 * 20) -- angleScale
        in
          position (moveHero hero (-1)) `shouldSatisfy` veryCloseToVector2 (Vector2 (-1) 0)

      it "should move forward correctly if facing west" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (180 * 20) -- angleScale
        in
          position (moveHero hero 1) `shouldSatisfy` veryCloseToVector2 (Vector2 (-1) 0)

      it "should move backward correctly if facing west" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (180 * 20) -- angleScale
        in
          position (moveHero hero (-1)) `shouldSatisfy` veryCloseToVector2 (Vector2 1 0)

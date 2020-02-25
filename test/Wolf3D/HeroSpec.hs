module Wolf3D.HeroSpec (heroSpec) where

import Test.Hspec
import Data.Vector
import Wolf3D.WorldData
import Wolf3D.Hero
import Wolf3D.SpecHelp


heroSpec :: SpecWith ()
heroSpec =
  describe "Wolf3D.Hero" $ do
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
          position (moveHero hero (-1)) `shouldSatisfy` veryCloseToVector2 (Vector2 1 0)

      it "should move backward correctly if facing east" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (0 * 20) -- angleScale
        in
          position (moveHero hero 1) `shouldSatisfy` veryCloseToVector2 (Vector2 (-1) 0)

      it "should move forward correctly if facing west" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (180 * 20) -- angleScale
        in
          position (moveHero hero (-1)) `shouldSatisfy` veryCloseToVector2 (Vector2 (-1) 0)

      it "should move backward correctly if facing west" $
        let
          hero = rotateHero (createHero (Vector2 0 0)) (180 * 20) -- angleScale
        in
          position (moveHero hero 1) `shouldSatisfy` veryCloseToVector2 (Vector2 1 0)
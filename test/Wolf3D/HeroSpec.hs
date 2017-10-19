module Wolf3D.HeroSpec (heroSpec) where

import Test.Hspec
import Data.Vector
import Wolf3D.Geom
import Wolf3D.Hero
import Wolf3D.SpecHelp


vec2Unit45 :: Double
vec2Unit45 = sqrt 0.5

heroSpec :: SpecWith ()
heroSpec = do
  describe "Wolf3D.Hero.heroLookRay" $ do
    it "should return correct length 1 for straight" $
      let
        hero = createHero (Vector2 0 0)
      in
        heroLookRay hero `shouldSatisfy` veryCloseToRay (createRay (Vector2 0 0) (Vector2 0 1))

    it "should return correct for looking backward" $
      let
        hero = rotateHero (createHero (Vector2 0 0)) pi
      in
        heroLookRay hero `shouldSatisfy` veryCloseToRay (createRay (Vector2 0 0) (Vector2 0 (-1)))

    it "should return correct for looking 45 right" $
      let
        hero = rotateHero (createHero (Vector2 0 0)) (pi / 4)
      in
        heroLookRay hero `shouldSatisfy` veryCloseToRay (createRay (Vector2 0 0) (Vector2 vec2Unit45 vec2Unit45))

    it "should return correct for looking 45 left" $
      let
        hero = rotateHero (createHero (Vector2 0 0)) (-pi / 4)
      in
        heroLookRay hero `shouldSatisfy` veryCloseToRay (createRay (Vector2 0 0) (Vector2 (-vec2Unit45) vec2Unit45))

  describe "Wolf3D.Hero.moveHero" $ do
    it "should move correctly if facing forward" $
      let
        hero = createHero (Vector2 0 0)
      in
        heroPosition (moveHero hero 1) `shouldSatisfy` veryCloseToVector2 (Vector2 0 1)

    it "should move correctly if facing backward" $
      let
        hero = rotateHero (createHero (Vector2 0 0)) pi
      in
        heroPosition (moveHero hero 1) `shouldSatisfy` veryCloseToVector2 (Vector2 0 (-1))


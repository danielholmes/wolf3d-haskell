module Wolf3D.HeroSpec (heroSpec) where

import Test.Hspec
import Data.Vector
import Wolf3D.Geom
import Wolf3D.Hero


heroSpec :: SpecWith ()
heroSpec =
  describe "Wolf3D.Hero.heroLookRay" $ do
    it "should return correct length 1 for straight" $
      let
        hero = createHero (Vector2 0 0)
      in
        heroLookRay hero `shouldBe` createRay (Vector2 0 0) (Vector2 0 1)

    it "should return correct for looking backward" $
      let
        hero = rotateHero (createHero (Vector2 0 0)) pi
      in
        heroLookRay hero `shouldBe` createRay (Vector2 0 0) (Vector2 0 (-1))

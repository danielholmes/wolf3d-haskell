module Wolf3D.WorldSpec (worldSpec) where

import Test.Hspec
import Data.Vector
import Wolf3D.WorldData
import Wolf3D.World
import Wolf3D.Hero
import Wolf3D.DataHelpers


dTileGlobalSize :: Double
dTileGlobalSize = fromIntegral tileGlobalSize

worldSpec :: SpecWith ()
worldSpec =
  describe "Wolf3D.World" $ do
    describe "isHittingWall" $ do
      it "should return false for middle of empty" $
        let
          wm = emptyWallMap 5 5
          pos = Vector2 dTileGlobalSize dTileGlobalSize
          hero = createHero pos
          w = createWorld GreyCeiling wm hero []
          result = isHittingWall w pos heroSize
        in
          result `shouldBe` False

      it "should return true for middle of full" $
        let
          wm = visualListToWallMap (replicate 5 (replicate 5 (Just Blue1)))
          pos = Vector2 dTileGlobalSize dTileGlobalSize
          hero = createHero pos
          w = createWorld GreyCeiling wm hero []
          result = isHittingWall w pos heroSize
        in
          result `shouldBe` True

      it "should return true for partial top left hit" $
        let
          wm = visualListToWallMap [[Just Blue1, Nothing, Nothing, Nothing, Nothing]
                                    , replicate 5 Nothing]
          pos = Vector2 (dTileGlobalSize - 1) (dTileGlobalSize - 1)
          hero = createHero pos
          w = createWorld GreyCeiling wm hero []
          result = isHittingWall w pos heroSize
        in
          result `shouldBe` True

      it "should return true for partial bottom right hit" $
        let
          wm = visualListToWallMap [[Nothing, Nothing]
                                    , [Nothing, Just Blue1]]
          pos = Vector2 (dTileGlobalSize - 1) (dTileGlobalSize - 1)
          hero = createHero pos
          w = createWorld GreyCeiling wm hero []
          result = isHittingWall w pos heroSize
        in
          result `shouldBe` True

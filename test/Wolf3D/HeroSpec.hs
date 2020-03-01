module Wolf3D.HeroSpec (heroSpec) where

import Test.Hspec
import Data.Vector
import Wolf3D.WorldData
import Wolf3D.World
import Wolf3D.Hero
import Wolf3D.SpecHelp


dTileSize :: Double
dTileSize = fromIntegral tileGlobalSize

heroPosUnit :: Vector2
heroPosUnit = Vector2 dTileSize dTileSize

heroSpec :: SpecWith ()
heroSpec =
  describe "Wolf3D.Hero" $ do
    describe "moveHero" $ do
      it "should move forward correctly if facing north" $
        let
          hero = rotateHero (createHero heroPosUnit) (90 * 20) -- angleScale
          world = createWorld GreyCeiling (emptyWallMap 2 2) hero []
        in
          position (moveHero world 1 hero) `shouldSatisfy` veryCloseToVector2 (Vector2 dTileSize (dTileSize - 1))

      it "should move forward correctly if facing south" $
        let
          hero = rotateHero (createHero heroPosUnit) (270 * 20) -- angleScale
          world = createWorld GreyCeiling (emptyWallMap 2 2) hero []
        in
          position (moveHero world 1 hero) `shouldSatisfy` veryCloseToVector2 (Vector2 dTileSize (dTileSize + 1))

      it "should move backward correctly if facing north" $
        let
          hero = rotateHero (createHero heroPosUnit) (90 * 20) -- angleScale
          world = createWorld GreyCeiling (emptyWallMap 2 2) hero []
        in
          position (moveHero world (-1) hero) `shouldSatisfy` veryCloseToVector2 (Vector2 dTileSize (dTileSize + 1))

      it "should move backward correctly if facing south" $
        let
          hero = rotateHero (createHero heroPosUnit) (270 * 20) -- angleScale
          world = createWorld GreyCeiling (emptyWallMap 2 2) hero []
        in
          position (moveHero world (-1) hero) `shouldSatisfy` veryCloseToVector2 (Vector2 dTileSize (dTileSize - 1))

      it "should move forward correctly if facing east" $
        let
          hero = rotateHero (createHero heroPosUnit) (0 * 20) -- angleScale
          world = createWorld GreyCeiling (emptyWallMap 2 2) hero []
        in
          position (moveHero world (-1) hero) `shouldSatisfy` veryCloseToVector2 (Vector2 (dTileSize + 1) dTileSize)

      it "should move backward correctly if facing east" $
        let
          hero = rotateHero (createHero heroPosUnit) (0 * 20) -- angleScale
          world = createWorld GreyCeiling (emptyWallMap 2 2) hero []
        in
          position (moveHero world 1 hero) `shouldSatisfy` veryCloseToVector2 (Vector2 (dTileSize - 1) dTileSize)

      it "should move forward correctly if facing west" $
        let
          hero = rotateHero (createHero heroPosUnit) (180 * 20) -- angleScale
          world = createWorld GreyCeiling (emptyWallMap 2 2) hero []
        in
          position (moveHero world (-1) hero) `shouldSatisfy` veryCloseToVector2 (Vector2 (dTileSize - 1) dTileSize)

      it "should move backward correctly if facing west" $
        let
          hero = rotateHero (createHero heroPosUnit) (180 * 20) -- angleScale
          world = createWorld GreyCeiling (emptyWallMap 2 2) hero []
        in
          position (moveHero world 1 hero) `shouldSatisfy` veryCloseToVector2 (Vector2 (dTileSize + 1) dTileSize)

module Wolf3D.EngineSpec (engineSpec) where

import Test.Hspec
import Wolf3D.Geom
import Wolf3D.Engine
import Data.Vector
import Data.Maybe


data TestSimEntity

instance SimEntity TestSimEntity where
  simUpdate _ i = i

engineSpec :: SpecWith ()
engineSpec =
  describe "Wolf3D.Engine" $ do
    describe "castRayToClosestWall" $ do
      it "should return empty if no walls" $
        let
          world = createWorld GreyCeiling [] [] :: World TestSimEntity
          result = castRayToClosestWall world (createRay (Vector2 0 (-30)) (Vector2 0 30))
        in
          isNothing result `shouldBe` True

-- Will be removed soon, so comment out for now
--      it "should return empty if miss walls" $
--        let
--          wm = [[Just Red]]
--          world = createWorld GreyCeiling wm [] :: World TestSimEntity
--          result = castRayToClosestWall world (createRay (Vector2 0 (-30)) (Vector2 0 30))
--        in
--          isNothing result `shouldBe` True
--
--      it "should return correct wall and position if hit" $
--        let
--          wall = Wall (Vector2 (-100) 100) (Vector2 200 0) Red
--          world = createWorld GreyCeiling [wall] [] :: World TestSimEntity
--          result = castRayToClosestWall world (createRay (Vector2 0 (-30)) (Vector2 0 30))
--        in
--          result `shouldBe` Just (WallHit wall (Vector2 0 100) 130)

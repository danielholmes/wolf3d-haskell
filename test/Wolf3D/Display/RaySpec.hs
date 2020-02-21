module Wolf3D.Display.RaySpec (raySpec) where

import Test.Hspec
-- import Wolf3D.Geom
import Wolf3D.Display.Ray
import Wolf3D.Engine
import Data.Vector
import Data.List
-- import Data.Maybe

halfTileGlobalSize :: Double
halfTileGlobalSize = fromIntegral (tileGlobalSize `div` 2)

raySpec :: SpecWith ()
raySpec =
  describe "Wolf3D.Display.Ray" $ do
    describe "castRay" $ do
--      it "should return nothing if no walls" $
--        let
--          wm = emptyWallMap 2 2
--          pos = Vector2 0 0
--          angle = 90
--        in
--          castRay wm pos angle `shouldBe` Nothing
--      
--      it "should return hit for straight east" $
--        let
--          wm = transpose [[Nothing, Nothing, Just Red]
--                        , [Nothing, Nothing, Just Blue]
--                        , [Nothing, Nothing, Just Blue]]
--          pos = Vector2 0 halfTileGlobalSize
--          angle = 0
--        in
--          castRay wm pos angle `shouldBe` Just (Hit {material=Red})
--      
--      it "should return hit for straight west" $
--        let
--          wm = transpose [[Just Red, Nothing, Nothing]
--                        , [Just Blue, Nothing, Nothing]
--                        , [Just Blue, Nothing, Nothing]]
--          pos = Vector2 (fromIntegral (3 * tileGlobalSize)) halfTileGlobalSize
--          angle = 180
--        in
--          castRay wm pos angle `shouldBe` Just (Hit {material=Red})
--      
--      it "should return hit for straight north" $
--        let
--          wm = transpose [[Just Red, Just Blue, Just Blue]
--                        , [Nothing, Nothing, Just Blue]
--                        , [Nothing, Nothing, Just Blue]]
--          pos = Vector2 halfTileGlobalSize (fromIntegral (3 * tileGlobalSize))
--          angle = 90
--        in
--          castRay wm pos angle `shouldBe` Just (Hit {material=Red})
--      
--      it "should return hit for straight south" $
--        let
--          wm = transpose [[Nothing, Nothing, Nothing]
--                        , [Nothing, Nothing, Nothing]
--                        , [Just Red, Just Blue, Just Blue]]
--          pos = Vector2 halfTileGlobalSize 0
--          angle = 270
--        in
--          castRay wm pos angle `shouldBe` Just (Hit {material=Red})
      
      it "should return hit for NE / 0-90" $
        let
          wm = transpose [[Nothing, Nothing, Just Red]
                        , [Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing]]
          pos = Vector2 halfTileGlobalSize (3.0 * fromIntegral tileGlobalSize)
          angle = 45
        in
          castRay wm pos angle `shouldBe` Just (Hit {material=Red})
      
--      it "should return hit for NW / 90-180" $
--        let
--          wm = transpose [[Just Red, Nothing, Nothing]
--                        , [Nothing, Nothing, Nothing]
--                        , [Nothing, Nothing, Nothing]]
--          pos = Vector2 (2.5 * fromIntegral tileGlobalSize) (3.0 * fromIntegral tileGlobalSize)
--          angle = 135
--        in
--          castRay wm pos angle `shouldBe` Just (Hit {material=Red})
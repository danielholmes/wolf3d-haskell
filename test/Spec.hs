import Test.Hspec

import Data.Time.Clock
import Data.Time.Calendar
import Data.Maybe
import Data.Vector
import Wolf3D.Runner
import Wolf3D.Display
import Wolf3D.Types
import Wolf3D.World

main :: IO ()
main = hspec $ do
  describe "Wolf3D.Runner.calculateTimerTickSpec" $ do
    it "should return no progression if same time as previous" $
      let
        now = UTCTime (ModifiedJulianDay 0) 0
        fixedStep = posInt 10
        maxSteps = posInt 3
        spec = calculateTimerTickSpec now fixedStep maxSteps now
      in
        spec `shouldBe` TimerTickSpec (posZInt 0) now

    it "should return previous time and no step if small progression" $
      let
        previous = UTCTime (ModifiedJulianDay 0) 0
        now = addUTCTime 0.00001 previous
        fixedStep = posInt 10
        maxSteps = posInt 3
        spec = calculateTimerTickSpec previous fixedStep maxSteps now
      in
        spec `shouldBe` TimerTickSpec (posZInt 0) previous

    it "should return correct time and step if exactly one step" $
      let
        previous = UTCTime (ModifiedJulianDay 0) 0
        now = addUTCTime 0.01 previous
        fixedStep = posInt 10
        maxSteps = posInt 3
        spec = calculateTimerTickSpec previous fixedStep maxSteps now
      in
        spec `shouldBe` TimerTickSpec (posZInt 1) now

    it "should return correct time and step if multiple steps" $
      let
        previous = UTCTime (ModifiedJulianDay 0) 0
        now = addUTCTime 0.025 previous
        expectedTime = addUTCTime 0.02 previous
        fixedStep = posInt 10
        maxSteps = posInt 3
        spec = calculateTimerTickSpec previous fixedStep maxSteps now
      in
        spec `shouldBe` TimerTickSpec (posZInt 2) expectedTime

    it "should return correct step and drop time if reach max steps" $
      let
        previous = UTCTime (ModifiedJulianDay 0) 0
        now = addUTCTime 1 previous
        fixedStep = posInt 10
        maxSteps = posInt 3
        spec = calculateTimerTickSpec previous fixedStep maxSteps now
      in
        spec `shouldBe` TimerTickSpec (posZInt 3) now

  describe "Wolf3D.Display.rayLineIntersection" $ do
    it "should return nothing if collinear" $
      let
        result = rayLineIntersection (Vector2 0 0, Vector2 0 1) (Vector2 0 2, Vector2 0 1)
      in
        isNothing result `shouldBe` True

    it "should return nothing if parallel" $
      let
        result = rayLineIntersection (Vector2 0 0, Vector2 0 1) (Vector2 1 0, Vector2 0 1)
      in
        isNothing result `shouldBe` True

    it "should return result if line-line intersection" $
      let
        result = rayLineIntersection (Vector2 0 0, Vector2 0 2) (Vector2 (-1) 1, Vector2 2 0)
      in
        result `shouldBe` Just (Vector2 0 1)

    it "should return result if ray-line intersection" $
      let
        result = rayLineIntersection (Vector2 0 0, Vector2 0 2) (Vector2 (-1) 3, Vector2 2 0)
      in
        result `shouldBe` Just (Vector2 0 3)

    it "should return nothing if regular non intersect" $
      let
        result = rayLineIntersection (Vector2 0 0, Vector2 0 2) (Vector2 1 1, Vector2 1 0)
      in
        isNothing result `shouldBe` True

  describe "Wolf3D.Display.castRayToClosestWall" $ do
    it "should return empty if no walls" $
      let
        world = createWorld []
        result = castRayToClosestWall world (Vector2 0 (-30), Vector2 0 30)
      in
        isNothing result `shouldBe` True

    it "should return empty if miss walls" $
      let
        world = createWorld [Wall (-100, -100) (200, 0) Red]
        result = castRayToClosestWall world (Vector2 0 (-30), Vector2 0 30)
      in
        isNothing result `shouldBe` True

    it "should return correct wall and position if hit" $
      let
        wall = Wall (-100, 100) (200, 0) Red
        world = createWorld [wall]
        result = castRayToClosestWall world (Vector2 0 (-30), Vector2 0 30)
      in
        result `shouldBe` Just (posZDouble 130, wall, Vector2 0 100)

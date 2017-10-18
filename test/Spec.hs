import Test.Hspec

import Data.Time.Clock
import Data.Time.Calendar
import Wolf3D.Runner

main :: IO ()
main = hspec $
  describe "Wolf3D.Runner.calculateTimerTickSpec" $ do
    it "should return no progression if same time as previous" $
      let
        now = UTCTime (ModifiedJulianDay 0) 0
        fixedStep = 10
        maxSteps = 3
        spec = calculateTimerTickSpec now fixedStep maxSteps now
      in
        spec `shouldBe` TimerTickSpec 0 0 now

    it "should return previous time and no step if small progression" $
      let
        previous = UTCTime (ModifiedJulianDay 0) 0
        now = addUTCTime 0.00001 previous
        fixedStep = 10
        maxSteps = 3
        spec = calculateTimerTickSpec previous fixedStep maxSteps now
      in
        spec `shouldBe` TimerTickSpec 0 0 previous

    it "should return correct time and step if exactly one step" $
      let
        previous = UTCTime (ModifiedJulianDay 0) 0
        now = addUTCTime 0.01 previous
        fixedStep = 10
        maxSteps = 3
        spec = calculateTimerTickSpec previous fixedStep maxSteps now
      in
        spec `shouldBe` TimerTickSpec 1 0 now

    it "should return correct time and step if multiple steps" $
      let
        previous = UTCTime (ModifiedJulianDay 0) 0
        now = addUTCTime 0.025 previous
        expectedTime = addUTCTime 0.02 previous
        fixedStep = 10
        maxSteps = 3
        spec = calculateTimerTickSpec previous fixedStep maxSteps now
      in
        spec `shouldBe` TimerTickSpec 2 0 expectedTime

    it "should return correct step and drop time if reach max steps" $
      let
        previous = UTCTime (ModifiedJulianDay 0) 0
        now = addUTCTime 1 previous
        fixedStep = 10
        maxSteps = 3
        spec = calculateTimerTickSpec previous fixedStep maxSteps now
      in
        spec `shouldBe` TimerTickSpec 3 0 now

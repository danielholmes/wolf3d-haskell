module Wolf3D.Runner (
  runLoop,
  timerTick,
  SimRun,
  startSimRun,
  simRunIsFinished,
  simRunWorld,
  TimerTickSpec (TimerTickSpec),
  calculateTimerTickSpec
) where

import Control.Monad.Loops (iterateUntilM)
import Data.Time.Clock
import Data.Maybe
import Wolf3D.World
import Wolf3D.Input
import Wolf3D.Sim
import Wolf3D.Hero


type FixedStepMillis = Int

data SimRun = SimRun World FixedStepMillis UTCTime Bool

runLoop :: World -> Int -> (World -> IO ()) -> IO SimRun
runLoop w s r = do
  simRun <- startSimRun w s
  iterateUntilM simRunIsFinished (timerTick r) simRun

startSimRun :: World -> FixedStepMillis -> IO SimRun
startSimRun w s = do
  startTime <- getCurrentTime
  return (SimRun w s startTime False)

simRunIsFinished :: SimRun -> Bool
simRunIsFinished (SimRun _ _ _ f) = f

simRunWorld :: SimRun -> World
simRunWorld (SimRun w _ _ _) = w

finishRun :: SimRun -> SimRun
finishRun (SimRun w f t _) = SimRun w f t True

type NumSteps = Int

data TimerTickSpec = TimerTickSpec NumSteps UTCTime
  deriving (Show, Eq)

calculateTimerTickSpec :: UTCTime -> FixedStepMillis -> Int -> UTCTime -> TimerTickSpec
calculateTimerTickSpec prev fixedStep maxSteps now = TimerTickSpec numSteps updatedTime
  where
    millisAvailable = diffUTCTimeMillis prev now
    numSteps = min maxSteps (millisAvailable `div` fixedStep)
    usedTime = realToFrac (numSteps * fixedStep) / 1000.0
    updatedTime
      | numSteps < maxSteps = addUTCTime usedTime prev
      | otherwise           = now

timerTick :: (World -> IO ()) -> SimRun -> IO SimRun
timerTick render run@(SimRun world fixedStep previousTime _) = do
  now <- getCurrentTime
  -- TODO: Lift to SimRun
  let maxStepsPerTick = 3
  let (TimerTickSpec numSteps updatedTime) = calculateTimerTickSpec previousTime fixedStep maxStepsPerTick now

  currentInput <- processInput (heroActionsState (worldHero world))
  let runWithInput = applyInput run currentInput
  let preTickWorld = simRunWorld runWithInput

  let ranWorld = tickWorldNTimes preTickWorld fixedStep numSteps

  case ranWorld of
    Just newWorld -> render newWorld
    Nothing -> return ()

  --TODO: Discard extra time IF frame rate running too slow. i.e. if numSteps == maxSteps then updatedTime should be now
  --SDL.delay 1000
  let runWithNewWorld = updateSimRunWorld runWithInput (fromMaybe preTickWorld ranWorld)
  let runWithNewTimes = updateSimRunTimes runWithNewWorld updatedTime
  return runWithNewTimes
  --traceShow (previousTime, now, millisAvailable, fixedStep, numSteps, updatedTime) (return runWithNewTimes)

applyInput :: SimRun -> InputState -> SimRun
applyInput run input
  | inputQuit input = finishRun run
  | otherwise       = updateSimRunPlayerActionsState run (inputHeroActionsState input)

diffUTCTimeMillis :: UTCTime -> UTCTime -> Int
diffUTCTimeMillis previous now = floor (diff * 1000)
  where
    diff = toRational (diffUTCTime now previous)

updateSimRunWorld :: SimRun -> World -> SimRun
updateSimRunWorld (SimRun _ s p f) newWorld = SimRun newWorld s p f

updateSimRunTimes :: SimRun -> UTCTime -> SimRun
updateSimRunTimes (SimRun w s _ f) newTime = SimRun w s newTime f

updateSimRunPlayerActionsState :: SimRun -> HeroActionsState -> SimRun
updateSimRunPlayerActionsState (SimRun w s t f) pas = SimRun (updateWorldHeroActionsState w pas) s t f

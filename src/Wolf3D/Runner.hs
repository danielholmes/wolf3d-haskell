module Wolf3D.Runner (
  timerTick,
  SimRun,
  startSimRun,
  simRunIsFinished,
  simRunWorld,
  TimerTickSpec (TimerTickSpec),
  calculateTimerTickSpec
) where

import Data.Time.Clock
import Data.Maybe

import Wolf3D.World
import Wolf3D.Input
import Wolf3D.Sim
import Wolf3D.Player

type FixedStepMillis = Int

data SimRun = SimRun PositionWorld FixedStepMillis UTCTime Bool

maxStepsPerTick :: Int
maxStepsPerTick = 3

startSimRun :: PositionWorld -> FixedStepMillis -> IO SimRun
startSimRun w s = do
  startTime <- getCurrentTime
  return (SimRun w s startTime False)

simRunIsFinished :: SimRun -> Bool
simRunIsFinished (SimRun _ _ _ f) = f

simRunWorld :: SimRun -> PositionWorld
simRunWorld (SimRun w _ _ _) = w

finishRun :: SimRun -> SimRun
finishRun (SimRun w f t _) = SimRun w f t True

type NumSteps = Int
type WaitMillis = Int

data TimerTickSpec = TimerTickSpec NumSteps WaitMillis UTCTime

instance Show TimerTickSpec where
  show (TimerTickSpec n w t) = "TimerTickSpec numSteps=" ++ show n ++ " wait=" ++ show w ++ " newTime=" ++ show t

instance Eq TimerTickSpec where
  (==) (TimerTickSpec n1 w1 t1) (TimerTickSpec n2 w2 t2) = n1 == n2 && w1 == w2 && t1 == t2

calculateTimerTickSpec :: UTCTime -> Int -> Int -> UTCTime -> TimerTickSpec
calculateTimerTickSpec prev fixedStep maxSteps now = TimerTickSpec numSteps 0 updatedTime
  where
    millisAvailable = diffUTCTimeMillis prev now
    numSteps = min maxSteps (millisAvailable `div` fixedStep)
    usedTime = realToFrac (numSteps * fixedStep) / 1000.0
    updatedTime
      | numSteps < maxSteps = addUTCTime usedTime prev
      | otherwise           = now

timerTick :: (PositionWorld -> IO ()) -> SimRun -> IO SimRun
timerTick render run@(SimRun world fixedStep previousTime _) = do
  now <- getCurrentTime
  let (TimerTickSpec numSteps _ updatedTime) = calculateTimerTickSpec previousTime fixedStep maxStepsPerTick now

  currentInput <- processInput (worldPlayerActionsState world)
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
  | otherwise       = updateSimRunPlayerActionsState run (inputPlayerActionsState input)

diffUTCTimeMillis :: UTCTime -> UTCTime -> Int
diffUTCTimeMillis previous now = diffMillis
  where
    diff = toRational (diffUTCTime now previous)
    diffMillis = floor (diff * 1000) :: Int

updateSimRunWorld :: SimRun -> PositionWorld -> SimRun
updateSimRunWorld (SimRun _ s p f) newWorld = SimRun newWorld s p f

updateSimRunTimes :: SimRun -> UTCTime -> SimRun
updateSimRunTimes (SimRun w s _ f) newTime = SimRun w s newTime f

updateSimRunPlayerActionsState :: SimRun -> PlayerActionsState -> SimRun
updateSimRunPlayerActionsState (SimRun w s t f) pas = SimRun (updateWorldPlayerActionsState w pas) s t f

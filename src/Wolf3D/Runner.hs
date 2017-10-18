module Wolf3D.Runner (timerTick, SimRun, startSimRun, simRunIsFinished, simRunWorld, finishRun) where

import Debug.Trace
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

timerTick :: (PositionWorld -> IO ()) -> SimRun -> IO SimRun
timerTick render run@(SimRun world fixedStep previousTime _) = do
  now <- getCurrentTime
  let millisAvailable = diffUTCTimeMillis previousTime now
  let numSteps = min maxStepsPerTick (millisAvailable `div` fixedStep)

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
  let usedTime = realToFrac (numSteps * fixedStep) / 1000.0
  let updatedTime = addUTCTime usedTime previousTime
  let runWithNewTimes = updateSimRunTimes runWithNewWorld updatedTime
  traceShow (previousTime, now, millisAvailable, fixedStep, numSteps, updatedTime) (return runWithNewTimes)

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

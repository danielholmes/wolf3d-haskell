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
import Wolf3D.Types


type FixedStepMillis = PosInt

data SimRun = SimRun PositionWorld FixedStepMillis UTCTime Bool

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

type NumSteps = PosZInt

data TimerTickSpec = TimerTickSpec NumSteps UTCTime

instance Show TimerTickSpec where
  show (TimerTickSpec n t) = "TimerTickSpec numSteps=" ++ show n ++ " newTime=" ++ show t

instance Eq TimerTickSpec where
  (==) (TimerTickSpec n1 t1) (TimerTickSpec n2 t2) = n1 == n2 && t1 == t2

calculateTimerTickSpec :: UTCTime -> FixedStepMillis -> PosInt -> UTCTime -> TimerTickSpec
calculateTimerTickSpec prev fixedStep maxSteps now = TimerTickSpec numSteps updatedTime
  where
    millisAvailable = diffUTCTimeMillis prev now
    numSteps = posZInt (min (fromPosInt maxSteps) (fromPosZInt millisAvailable `div` fromPosInt fixedStep))
    usedTime = realToFrac (fromPosZInt numSteps * fromPosInt fixedStep) / 1000.0
    updatedTime
      | fromPosZInt numSteps < fromPosInt maxSteps = addUTCTime usedTime prev
      | otherwise                                  = now

timerTick :: (PositionWorld -> IO ()) -> SimRun -> IO SimRun
timerTick render run@(SimRun world fixedStep previousTime _) = do
  now <- getCurrentTime
  -- TODO: Lift to SimRun
  let maxStepsPerTick = posInt 3
  let (TimerTickSpec numSteps updatedTime) = calculateTimerTickSpec previousTime fixedStep maxStepsPerTick now

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

diffUTCTimeMillis :: UTCTime -> UTCTime -> PosZInt
diffUTCTimeMillis previous now = posZInt diffMillis
  where
    diff = toRational (diffUTCTime now previous)
    diffMillis = floor (diff * 1000) :: Int

updateSimRunWorld :: SimRun -> PositionWorld -> SimRun
updateSimRunWorld (SimRun _ s p f) newWorld = SimRun newWorld s p f

updateSimRunTimes :: SimRun -> UTCTime -> SimRun
updateSimRunTimes (SimRun w s _ f) newTime = SimRun w s newTime f

updateSimRunPlayerActionsState :: SimRun -> PlayerActionsState -> SimRun
updateSimRunPlayerActionsState (SimRun w s t f) pas = SimRun (updateWorldPlayerActionsState w pas) s t f
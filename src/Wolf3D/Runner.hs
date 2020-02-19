{-# LANGUAGE GADTs #-}
module Wolf3D.Runner (
  runLoop,
  timerTick,
  SimRun,
  simRunIsFinished,
  simRunWorld,
  TimerTickSpec (TimerTickSpec),
  calculateTimerTickSpec
) where

import Control.Monad.Loops (iterateUntilM)
import Data.Time.Clock
import Data.Maybe
import Wolf3D.Engine
import Wolf3D.Utils
import Wolf3D.Input
import Wolf3D.Sim
import Data.Foldable


type FixedStepMillis = Int
type MaxStepsPerTick = Int

data SimRun = SimRun (World Wolf3DSimEntity) FixedStepMillis MaxStepsPerTick UTCTime Bool

-- Note that wolf was Indeterministic (except for when demos were running)
-- But have set this up as deterministic, for now
runLoop :: World Wolf3DSimEntity -> FixedStepMillis -> MaxStepsPerTick -> (SimRun -> IO ()) -> IO SimRun
runLoop w f m r = do
  simRun <- startSimRun w f m
  iterateUntilM simRunIsFinished (timerTick r) simRun

startSimRun :: World Wolf3DSimEntity -> FixedStepMillis -> MaxStepsPerTick -> IO SimRun
startSimRun w f m = do
  startTime <- getCurrentTime
  return (SimRun w f m startTime False)

simRunIsFinished :: SimRun -> Bool
simRunIsFinished (SimRun _ _ _ _ f) = f

simRunWorld :: SimRun -> World Wolf3DSimEntity
simRunWorld (SimRun w _ _ _ _) = w

finishRun :: SimRun -> SimRun
finishRun (SimRun w f m t _) = SimRun w f m t True

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

timerTick :: (SimRun -> IO ()) -> SimRun -> IO SimRun
timerTick render run@(SimRun world fixedStep maxStepsPerTick previousTime _) = do
  now <- getCurrentTime
  let (TimerTickSpec numSteps updatedTime) = calculateTimerTickSpec previousTime fixedStep maxStepsPerTick now

  currentInput <- processInput (actionsState (worldHero world))
  let runWithInput = applyInput run currentInput
  let preTickWorld = simRunWorld runWithInput

  let ranWorld = tickWorldNTimes preTickWorld numSteps

  --TODO: Discard extra time IF frame rate running too slow. i.e. if numSteps == maxSteps then updatedTime should be now
  --SDL.delay 1000
  let runWithNewWorld = updateSimRunWorld runWithInput (fromMaybe preTickWorld ranWorld)
  let runWithNewTimes = updateSimRunTimes runWithNewWorld updatedTime

  forM_ (fmap (const runWithNewTimes) ranWorld) render

  return runWithNewTimes

applyInput :: SimRun -> InputState -> SimRun
applyInput run input
  | inputQuit input = finishRun run
  | otherwise       = updateSimRunPlayerActionsState run (inputHeroActionsState input)

updateSimRunWorld :: SimRun -> World Wolf3DSimEntity -> SimRun
updateSimRunWorld (SimRun _ s m p f) newWorld = SimRun newWorld s m p f

updateSimRunTimes :: SimRun -> UTCTime -> SimRun
updateSimRunTimes (SimRun w s m _ f) newTime = SimRun w s m newTime f

updateSimRunPlayerActionsState :: SimRun -> HeroActionsState -> SimRun
updateSimRunPlayerActionsState (SimRun w s m t f) has = SimRun (updateWorldHeroActionsState w has) s m t f

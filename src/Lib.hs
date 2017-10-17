{-# LANGUAGE OverloadedStrings #-}
module Lib (main) where

import qualified SDL
import qualified SDL.Image

import Control.Monad.Loops (iterateUntilM)
import Debug.Trace
import Data.Time.Clock

import Wolf3D.World
import Wolf3D.Input
import Wolf3D.Display
import Wolf3D.UI

quit :: PositionWorld -> Bool
quit (PositionWorld _ _ r) = r

gameLoop :: PositionWorld -> PositionWorld
gameLoop (PositionWorld (x,y) d@(dx,dy) r) = PositionWorld (x+dx,y+dy) d r

timerTick :: (PositionWorld -> IO ()) -> PositionWorld -> IO PositionWorld
timerTick r w = do
  start <- getCurrentTime
  wWithInput <- processInput w
  let newW = gameLoop wWithInput
  r newW
  --SDL.delay 1000
  end <- getCurrentTime
  let took = diffUTCTime end start
  traceShow (1 / took) (return newW)

main :: IO ()
main = createUI $
  \r -> do
    texture <- SDL.Image.loadTexture r "./assets/walk.png"

    _ <- iterateUntilM quit (timerTick (render texture r)) initWorld

    --        loopM (timerTick (render texture r)) initWorld

    --        let whileTick = C.isContinue <$> SDL.pollEvent >>= C.conditionallyRun (render texture r initWorld)
    --        whileM whileTick

    SDL.destroyTexture texture

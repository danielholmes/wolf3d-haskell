{-# LANGUAGE OverloadedStrings #-}
module Wolf3D (main) where

import qualified SDL
import qualified SDL.Image
import Control.Monad.Loops (iterateUntilM)

import Wolf3D.World
import Wolf3D.Display
import Wolf3D.UI
import Wolf3D.Runner
import Wolf3D.Types

main :: IO ()
main = createUI $
  \r -> do
    texture <- SDL.Image.loadTexture r "./assets/walk.png"
    let walls = [ Wall (-1000, 1000) (950, 0)
                , Wall (50, 1000) (950, 0)
                , Wall (-50, 1000) (0, 1000)
                , Wall (50, 1000) (0, 1000)]
    simRun <- startSimRun (createWorld walls) (posInt 16)
    _ <- iterateUntilM simRunIsFinished (timerTick (render texture r)) simRun
    SDL.destroyTexture texture

--        loopM (timerTick (render texture r)) initWorld

--        let whileTick = C.isContinue <$> SDL.pollEvent >>= C.conditionallyRun (render texture r initWorld)
--        whileM whileTick

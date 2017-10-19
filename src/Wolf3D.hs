{-# LANGUAGE OverloadedStrings #-}
module Wolf3D (main) where

import qualified SDL
import qualified SDL.Image
import Control.Monad.Loops (iterateUntilM)
import Data.Vector
import Wolf3D.World
import Wolf3D.Display
import Wolf3D.UI
import Wolf3D.Runner
import Wolf3D.Types

main :: IO ()
main = createUI $
  \r -> do
    texture <- SDL.Image.loadTexture r "./assets/walk.png"
    let walls = [ Wall (Vector2 (-1000) (-1000)) (Vector2 0 2000) Red
                , Wall (Vector2 (-1000) 1000) (Vector2 950 0) Green
                , Wall (Vector2 (-50) 1000) (Vector2 0 1000) Blue
                , Wall (Vector2 (-50) 2000) (Vector2 100 0) Red
                , Wall (Vector2 50 2000) (Vector2 0 (-1000)) Green
                , Wall (Vector2 50 1000) (Vector2 950 0) Blue
                , Wall (Vector2 1000 1000) (Vector2 0 (-2000)) Red]
    simRun <- startSimRun (createWorld walls) (posInt 16)
    _ <- iterateUntilM simRunIsFinished (timerTick (render texture r)) simRun
    SDL.destroyTexture texture

--        loopM (timerTick (render texture r)) initWorld

--        let whileTick = C.isContinue <$> SDL.pollEvent >>= C.conditionallyRun (render texture r initWorld)
--        whileM whileTick

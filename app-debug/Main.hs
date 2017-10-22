module Main where

import Wolf3D.UI
import Wolf3D.Geom
import Wolf3D.Runner
import Wolf3D.Debug.Data
import Wolf3D.Debug.Dummy
import Wolf3D.Debug.Display
import Data.Vector

main :: IO ()
main = do
  let size = Vector2 640 480
  createUI (roundToTuple size) $
    \r -> do
      setupRenderer r
      d <- loadRenderData r size
      runLoop dummyWorld 16 3 (render r d)

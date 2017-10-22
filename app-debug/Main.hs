module Main where

import Wolf3D.UI
import Wolf3D.Runner
import Wolf3D.Debug.Data
import Wolf3D.Debug.Dummy
import Wolf3D.Debug.Display

main :: IO ()
main = do
  let size = (640, 480)
  createUI size $
    \r -> do
      setupRenderer r
      d <- loadRenderData r size
      runLoop dummyWorld2 16 3 (render r d)

module Main where

import Wolf3D.Display
import Wolf3D.UI
import Wolf3D.Runner
import Wolf3D.Loader
import Wolf3D.Debug.Dummy

main :: IO ()
main = do
  let size = (640, 480)
  createUI size $
    \r -> do
      setupRenderer r
      d <- loadRenderData r size
      runLoop dummyWorld 16 3 (render r d)

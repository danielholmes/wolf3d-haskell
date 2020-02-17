module Wolf3D.Main (
  createMain
) where

import Wolf3D.Runner
import Wolf3D.UI
import Wolf3D.Engine
import Wolf3D.Sim
import qualified SDL
import Data.StateVar (($=))

type Scale = Int

createMain :: Scale -> World Wolf3DSimEntity -> (SDL.Renderer -> (Int, Int) -> IO (SimRun -> IO ())) -> IO ()
createMain s initWorld createRender = do
  -- Original native size
  let width = s * 320
  let height = s * 200
  -- wolf3d was 320 x 200 on 4:3 displays, so it was scaled vertically
  let windowSize = (width, width `div` 4 * 3) :: (Int, Int)
  -- On a machine with infinite resources, frame rate of Wolf3D was 70fps
  let frameTime = 14
  createUI windowSize $
    \r -> do
      render <- createRender r (width, height)
      let scaleX = 1.0 -- (windowWidth / width) / 1.0
      let scaleY = 1.2 -- (windowHeight / height) / 1.0
      SDL.rendererScale r $= (SDL.V2 scaleX scaleY)
      runLoop initWorld frameTime 3 render

module Wolf3D.Main (
  createMain
) where

import Wolf3D.Runner
import Wolf3D.UI
import Wolf3D.Engine
import Wolf3D.Sim
import Wolf3D.Display (screenWidth, screenHeight)
import qualified SDL
import Data.StateVar (($=))

type Scale = Int

createMain :: Scale -> World Wolf3DSimEntity -> (SDL.Renderer -> IO (SimRun -> IO ())) -> IO ()
createMain s initWorld createRender = do
  -- Original native size
  let width = s * screenWidth
  -- wolf3d was 320 x 200 on 4:3 displays, so it was scaled vertically
  let windowSize = (width, width `div` 4 * 3)
  -- On a machine with infinite resources, frame rate of Wolf3D was 70fps
  let frameTime = 14
  createUI windowSize $
    \r -> do
      render <- createRender r
      let fWindowWidth = (fromIntegral (fst windowSize))
      let fWindowHeight = (fromIntegral (snd windowSize))
      let fScreenWidth = (fromIntegral screenWidth)
      let fScreenHeight = (fromIntegral screenHeight)
      let scaleX = fWindowWidth / fScreenWidth
      let scaleY = fWindowHeight / fScreenHeight
      SDL.rendererScale r $= (SDL.V2 scaleX scaleY)
      runLoop initWorld frameTime 3 render

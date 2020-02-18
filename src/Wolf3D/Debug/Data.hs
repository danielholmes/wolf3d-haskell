module Wolf3D.Debug.Data (loadRenderData) where

import qualified SDL
import qualified SDL.Font
import Wolf3D.Debug.Display
import qualified Wolf3D.Loader as D

loadRenderData :: SDL.Renderer -> IO DebugRenderData
loadRenderData r = do
  d <- D.loadRenderData r
  font <- SDL.Font.load "assets/Monospace.ttf" 12
  return (DebugRenderData d font)

module Wolf3D.Debug.Data (loadRenderData) where

import qualified SDL
import qualified SDL.Font
import Wolf3D.Debug.Display
import qualified Wolf3D.Data as D
import Data.Vector

loadRenderData :: SDL.Renderer -> Vector2 -> IO DebugRenderData
loadRenderData r s = do
  d <- D.loadRenderData r s
  font <- SDL.Font.load "assets/Monospace.ttf" 12
  return (DebugRenderData d font)
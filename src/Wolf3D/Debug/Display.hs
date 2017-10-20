module Wolf3D.Debug.Display (
  setupRenderer,
  render
) where

import qualified Wolf3D.Display as D
import Wolf3D.World
import Wolf3D.Types
import Wolf3D.SDLUtils
import Wolf3D.Display.MiniMap
import Wolf3D.Display.Utils
import qualified SDL
import Data.Vector
import Data.StateVar (($=))


setupRenderer :: SDL.Renderer -> IO ()
setupRenderer r = do
  D.setupRenderer r
  SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend

render :: SDL.Renderer -> World -> IO ()
render r w = do
  D.renderWorld r w
  let size = Vector2 192 144
  withViewport r (Just (mkOriginSDLRect size)) $
    renderMiniMap r (posDouble 0.009) size w
  SDL.present r

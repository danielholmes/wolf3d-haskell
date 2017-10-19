module Wolf3D.Debug.Display (
  setupRenderer,
  render
) where

import qualified Wolf3D.Display as D
import Wolf3D.World
import Wolf3D.Hero
import Wolf3D.SDLUtils
import qualified SDL
import Data.Vector
import Data.StateVar (($=))
import Foreign.C.Types (CInt)


setupRenderer :: SDL.Renderer -> IO ()
setupRenderer r = do
  D.setupRenderer r
  SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend

render :: SDL.Renderer -> World -> IO ()
render r w = do
  D.renderWorld r w
  --let oldViewport = get (SDL.rendererViewport r)
  let size = Vector2 128 96
  SDL.rendererViewport r $= Just (mkOriginSDLRect size)
  renderMiniMap r size w
  SDL.rendererViewport r $= Nothing
  SDL.present r

renderMiniMap :: SDL.Renderer -> Vector2 -> World -> IO ()
renderMiniMap r size w = do
  SDL.rendererDrawColor r $= SDL.V4 0 0 0 100
  SDL.fillRect r (Just (mkOriginSDLRect size))
  renderMiniMapHero r size (worldHero w)

renderMiniMapHero :: SDL.Renderer -> Vector2 -> Hero -> IO ()
renderMiniMapHero r (Vector2 w h) _ = do
    SDL.rendererDrawColor r $= SDL.V4 255 0 0 255
    SDL.drawRect r (Just (mkSDLRect x y size size))
  where
    size :: CInt
    size = 10
    halfSize :: CInt
    halfSize = size `div` 2
    x = (round w `div` 2) - halfSize
    y = (round h `div` 2) - halfSize

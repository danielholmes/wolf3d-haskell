module Wolf3D.Display.MiniMap (renderMiniMap) where

import qualified SDL
import Data.Vector
import Wolf3D.Hero
import Wolf3D.World
import Wolf3D.Types
import Wolf3D.Geom
import Wolf3D.SDLUtils
import Data.Foldable (forM_)
import Data.StateVar (($=))


renderMiniMap :: SDL.Renderer -> PosDouble -> Vector2 -> World -> IO ()
renderMiniMap r scale size w = do
  SDL.rendererDrawColor r $= SDL.V4 0 0 0 100
  SDL.fillRect r (Just (mkOriginSDLRect size))
  renderHero r (fromPosDouble scale) size (worldHero w)
  renderWalls r (fromPosDouble scale) size w

renderHero :: SDL.Renderer -> Double -> Vector2 -> Hero -> IO ()
renderHero r scale s h = do
  SDL.rendererDrawColor r $= SDL.V4 255 0 0 255
  drawEqTriangle r (scale * heroSize) halfSize rotation
  SDL.drawLine r (roundToSDLP halfSize) (roundToSDLP (halfSize - rotateVector2 (Vector2 0 (heroSize / 2) *| scale) rotation))
  where
    heroSize = 1000
    halfSize = s *| 0.5
    rotation = - (heroRotation h)

renderWalls :: SDL.Renderer -> Double -> Vector2 -> World -> IO ()
renderWalls r scale size w = do
  SDL.rendererDrawColor r $= SDL.V4 0 255 0 255
  forM_ walls (renderWall r scale size position)
  where
    position = worldHeroPosition w
    --flipY = Vector2 (-1) 1
    worldSize = size *| (1 / scale)
    worldHalfSize = worldSize *| 0.5
    worldRect = Rectangle (position - worldHalfSize) worldSize
    walls = worldWallsTouching w worldRect

renderWall :: SDL.Renderer -> Double -> Vector2 -> Vector2 -> Wall -> IO ()
renderWall r scale size heroPos (Wall o s _) = SDL.drawLine r from to
  where
    halfSize = size *| 0.5
    flipY = Vector2 1 (-1)
    oFromHero = o - heroPos
    sFromHero = oFromHero + s
    from = roundToSDLP (halfSize + (flipY * oFromHero *| scale))
    to = roundToSDLP (halfSize + (flipY * sFromHero *| scale))

cos60 :: Double
cos60 = cos (pi / 3)

drawEqTriangle :: SDL.Renderer -> Double -> Vector2 -> Double -> IO ()
drawEqTriangle r s pos rot = do
  SDL.drawLine r top right
  SDL.drawLine r right left
  SDL.drawLine r left top
  where
    halfHeight = s * cos60 / 2
    bottomY = halfHeight
    topY = -halfHeight
    rightX = halfHeight
    leftX = -halfHeight
    top = roundToSDLP (pos + rotateVector2 (Vector2 0 topY) rot)
    right = roundToSDLP (pos + rotateVector2 (Vector2 rightX bottomY) rot)
    left = roundToSDLP (pos + rotateVector2 (Vector2 leftX bottomY) rot)

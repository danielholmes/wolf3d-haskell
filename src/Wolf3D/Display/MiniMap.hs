module Wolf3D.Display.MiniMap (renderMiniMap) where

import qualified SDL
import qualified SDL.Vect
import Foreign.C.Types (CInt)
import Data.Vector
import Wolf3D.Hero
import Wolf3D.World
import Wolf3D.Types
import Wolf3D.Items
import Wolf3D.Geom
import Wolf3D.SDLUtils
import Data.Foldable (forM_)
import Data.StateVar (($=))


data MiniMapData = MiniMapData Double Vector2 Rectangle Vector2

renderMiniMap :: SDL.Renderer -> PosDouble -> Vector2 -> World -> IO ()
renderMiniMap r dScale size w = do
  SDL.rendererDrawColor r $= SDL.V4 0 0 0 100
  SDL.fillRect r (Just (mkOriginSDLRect size))
  renderHero r mMData (worldHero w)
  renderItems r mMData w
  renderWalls r mMData w
  where
    mMData = createMiniMapData dScale size w

createMiniMapData :: PosDouble -> Vector2 -> World -> MiniMapData
createMiniMapData dScale size w = MiniMapData scale size worldRect hPosition
  where
    hPosition = worldHeroPosition w
    scale = fromPosDouble dScale
    worldSize = size *| (1 / scale)
    worldHalfSize = worldSize *| 0.5
    worldRect = Rectangle (hPosition - worldHalfSize) worldSize

renderHero :: SDL.Renderer -> MiniMapData -> Hero -> IO ()
renderHero r d@(MiniMapData scale size _ heroPos) h = do
  SDL.rendererDrawColor r $= SDL.V4 255 0 0 255
  drawEqTriangle r (scale * heroSize) halfSize (-rotation)
  drawMiniMapLine r d (heroPos, rotateVector2 (Vector2 0 heroSize / 2) rotation)
  where
    heroSize = 1000
    halfSize = size *| 0.5
    rotation = heroRotation h

renderWalls :: SDL.Renderer -> MiniMapData -> World -> IO ()
renderWalls r d@(MiniMapData _ _ worldRect _) w = do
  SDL.rendererDrawColor r $= SDL.V4 0 255 0 255
  forM_ (worldWallsTouching w worldRect) (renderWall r d)

renderWall :: SDL.Renderer -> MiniMapData -> Wall -> IO ()
renderWall r d (Wall o s _) = drawMiniMapLine r d (o, s)

drawMiniMapLine :: SDL.Renderer -> MiniMapData -> Line -> IO ()
drawMiniMapLine r d (o, s) = SDL.drawLine r (toMiniMapP d o) (toMiniMapP d (o + s))

toMiniMapP :: MiniMapData -> Vector2 -> SDL.Vect.Point SDL.Vect.V2 CInt
toMiniMapP (MiniMapData scale size _ heroPos) v = roundToSDLP (halfSize + (flipY * (v - heroPos) *| scale))
  where
    flipY = Vector2 1 (-1)
    halfSize = size *| 0.5

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

drawRectangle :: SDL.Renderer -> MiniMapData -> Rectangle -> IO ()
drawRectangle r d rect = forM_ (rectangleSides rect) (drawMiniMapLine r d)

renderItems :: SDL.Renderer -> MiniMapData -> World -> IO ()
renderItems r d@(MiniMapData _ _ worldRect _) w = do
  SDL.rendererDrawColor r $= SDL.V4 0 0 255 255
  forM_ items (renderItem r d)
  where
    items = worldItemsTouching w worldRect

renderItem :: SDL.Renderer -> MiniMapData -> Item -> IO ()
renderItem r d i = drawRectangle r d (itemRectangle i)

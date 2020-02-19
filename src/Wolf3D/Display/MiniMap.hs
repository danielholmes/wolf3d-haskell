module Wolf3D.Display.MiniMap (renderMiniMap) where

import qualified SDL
import qualified SDL.Vect
import Foreign.C.Types (CInt)
import Data.Vector
import Wolf3D.Engine
import Wolf3D.Geom
import Wolf3D.Sim
import Wolf3D.SDLUtils
import Data.Foldable (forM_)
import Data.StateVar (($=))
import Data.Maybe (fromJust)
import Data.List (find)
import GHC.Word (Word8)


data MiniMapData = MiniMapData Double (CInt, CInt) Vector2 Rectangle Vector2

heroColor :: SDL.V4 Word8
heroColor = SDL.V4 255 0 0 255

originColor :: SDL.V4 Word8
originColor = SDL.V4 255 255 0 255

wallColor :: SDL.V4 Word8
wallColor = SDL.V4 0 255 0 255

itemColor :: SDL.V4 Word8
itemColor = SDL.V4 0 0 255 255

panelColor :: SDL.V4 Word8
panelColor = SDL.V4 0 0 0 100

renderMiniMap :: SDL.Renderer -> Double -> (CInt, CInt) -> World Wolf3DSimEntity -> IO ()
renderMiniMap r dScale size@(width, height) w = do
  SDL.rendererDrawColor r $= panelColor
  SDL.fillRect r (Just (mkSDLRect 0 0 width height))
  renderHero r mMData hero
  renderItems r mMData w
  renderWalls r mMData w
  renderOrigin r mMData
  where
    hero = fromJust (fmap (\(SEHero h) -> h) (find (\i -> case i of (SEHero _) -> True; _ -> False) (worldEntities w)))
    mMData = createMiniMapData dScale size w

createMiniMapData :: Double -> (CInt, CInt) -> World Wolf3DSimEntity -> MiniMapData
createMiniMapData scale size@(width, height) w = MiniMapData scale size halfSize worldRect hPosition
  where
    hero = fromJust (fmap (\(SEHero h) -> h) (find (\i -> case i of (SEHero _) -> True; _ -> False) (worldEntities w)))
    hPosition = position hero
    halfSize = Vector2 (fromIntegral (width `div` 2)) (fromIntegral (height `div` 2))
    worldSize = Vector2 (fromIntegral width) (fromIntegral height) *| (1 / scale)
    worldHalfSize = worldSize *| 0.5
    worldRect = Rectangle (hPosition - worldHalfSize) worldSize

renderHero :: SDL.Renderer -> MiniMapData -> Hero -> IO ()
renderHero r d@(MiniMapData scale _ halfSize _ heroPos) h = do
  SDL.rendererDrawColor r $= heroColor
  drawEqTriangle r (scale * heroSize) halfSize alignedRot1
  drawMiniMapLine r d (heroPos, rotateVector2 (Vector2 0 heroSize / 2) alignedRot2)
  where
    -- TODO: Take something from sim about this
    heroSize = 40000
    rotation = snappedRotation h
    rotationRads = deg2Rad * (fromIntegral rotation)
    alignedRot1 = rotationRads - pi / 2
    alignedRot2 = rotationRads + pi / 2

renderWalls :: SDL.Renderer -> MiniMapData -> World Wolf3DSimEntity -> IO ()
renderWalls r d@(MiniMapData _ _ _ worldRect _) w = do
  SDL.rendererDrawColor r $= wallColor
  forM_ (worldWallsTouching w worldRect) (renderWall r d)

renderWall :: SDL.Renderer -> MiniMapData -> Wall -> IO ()
renderWall r d (Wall o s _) = drawMiniMapLine r d (o, s)

renderOrigin :: SDL.Renderer -> MiniMapData -> IO ()
renderOrigin r d = do
  SDL.rendererDrawColor r $= originColor
  let halfTileGlobalSize = fromIntegral (tileGlobalSize `div` 2)
  drawMiniMapLine r d (Vector2 (-halfTileGlobalSize) 0, Vector2 (fromIntegral tileGlobalSize) 0)
  drawMiniMapLine r d (Vector2 0 (-halfTileGlobalSize), Vector2 0 (fromIntegral tileGlobalSize))

drawMiniMapLine :: SDL.Renderer -> MiniMapData -> Line -> IO ()
drawMiniMapLine r d (o, s) = SDL.drawLine r (toMiniMapP d o) (toMiniMapP d (o + s))

toMiniMapP :: MiniMapData -> Vector2 -> SDL.Vect.Point SDL.Vect.V2 CInt
toMiniMapP (MiniMapData scale _ halfSize _ heroPos) v = roundToSDLP (halfSize + (v - heroPos) *| scale)

cos60 :: Double
cos60 = cos (pi / 3)

drawEqTriangle :: SDL.Renderer -> Double -> Vector2 -> Double -> IO ()
drawEqTriangle r s pos rotRads = do
  SDL.drawLine r top right
  SDL.drawLine r right left
  SDL.drawLine r left top
  where
    halfHeight = s * cos60 / 2
    bottomY = halfHeight
    topY = -halfHeight
    rightX = halfHeight
    leftX = -halfHeight
    top = roundToSDLP (pos + rotateVector2 (Vector2 0 topY) rotRads)
    right = roundToSDLP (pos + rotateVector2 (Vector2 rightX bottomY) rotRads)
    left = roundToSDLP (pos + rotateVector2 (Vector2 leftX bottomY) rotRads)

drawRectangle :: SDL.Renderer -> MiniMapData -> Rectangle -> IO ()
drawRectangle r d rect = forM_ (rectangleSides rect) (drawMiniMapLine r d)

renderItems :: SDL.Renderer -> MiniMapData -> World Wolf3DSimEntity -> IO ()
renderItems r d@(MiniMapData _ _ _ worldRect _) w = do
  SDL.rendererDrawColor r $= itemColor
  forM_ (worldEnvItemsTouching worldRect w) (renderItem r d)

renderItem :: SDL.Renderer -> MiniMapData -> EnvItem -> IO ()
renderItem r d i = drawRectangle r d (itemRectangle i)

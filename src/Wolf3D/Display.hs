module Wolf3D.Display (
  setupRenderer,
  render,
  renderWorld,
  renderHud,
  RenderData (RenderData),
  WallMaterialData,
  screenWidth,
  screenHeight,
  actionHeight,
  actionWidth
) where

import Wolf3D.Sim
import Wolf3D.Engine
import Wolf3D.Runner
import Wolf3D.SDLUtils
import Wolf3D.Display.Utils
import Wolf3D.Display.Hud
import Wolf3D.Display.Ray
import Wolf3D.Animation
import qualified SDL
import Data.StateVar (($=))
import Data.Vector
import Data.Foldable
import Data.Maybe
import Control.Monad (mfilter)
import qualified Data.Map as M
import Foreign.C.Types (CInt)
import GHC.Word (Word8)
import Wolf3D.Display.Data


-- deprecated, not sure what it is actually
distToProjPlane :: Double
distToProjPlane = fromIntegral (actionWidth `div` 2) / (tan (pi / 96))

ceilingColors :: M.Map Ceiling (SDL.V4 Word8)
ceilingColors = M.fromList [(GreyCeiling, SDL.V4 55 55 55 255)
                            , (PurpleCeiling, SDL.V4 63 2 63 255)
                            , (GreenCeiling, SDL.V4 0 110 110 255)
                            , (YellowCeiling, SDL.V4 87 83 2 255)]

floorColor :: SDL.V4 Word8
floorColor = SDL.V4 112 112 112 255

setupRenderer :: SDL.Renderer -> IO ()
setupRenderer r = SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend

render :: SDL.Renderer -> RenderData -> SimRun -> IO ()
render r d s = do
  renderHud r d
  renderWorld r d (simRunWorld s)
  SDL.present r

renderWorld :: SDL.Renderer -> RenderData -> World Wolf3DSimEntity -> IO ()
renderWorld r d w = do
  renderCeilingAndFloor r d w
  renderWalls r d w
  renderWeapon r d (worldTics w) (worldHeroWeapon w)

renderCeilingAndFloor :: SDL.Renderer -> RenderData -> World Wolf3DSimEntity -> IO ()
renderCeilingAndFloor r _ w = do
  SDL.rendererDrawColor r $= ceilingColor
  SDL.fillRect r (Just (mkSDLRect actionAreaX actionAreaY actionWidth halfActionHeight))
  SDL.rendererDrawColor r $= floorColor
  SDL.fillRect r (Just (mkSDLRect actionAreaX (actionAreaY + halfActionHeight) actionWidth halfActionHeight))
  where
    ceilingColor = fromJust (M.lookup (worldCeilingColor w) ceilingColors)

renderWalls :: SDL.Renderer -> RenderData -> World Wolf3DSimEntity -> IO ()
renderWalls r d w = forM_ (zip [0..] hits) (renderWallLine r d hero)
  where
    hero = worldHero w
    heroFineAngle = normalToFineAngle (snappedRotation hero)
    hits = castRaysToWalls (worldWallMap w) (position hero) heroFineAngle

renderWallLine :: SDL.Renderer -> RenderData -> Hero -> (CInt, WallRayHit) -> IO ()
renderWallLine r (RenderData {wallTextures=wt}) hero (pixel, WallRayHit {material=m, tilePosition=tilePos, intercept=(x, y)}) = do
  SDL.copy r texture sourceRect destRect
  where
    (texture, (textureWidth, textureHeight)) = fromJust (M.lookup m wt)
    hitWallTextureRatio = (fromIntegral tilePos / fromIntegral tileGlobalSize) :: Double
    
    -- TODO: Remove this, check orig source
    vectorDist :: Vector2 -> Vector2 -> Double
    vectorDist (Vector2 x1 y1) (Vector2 x2 y2) = sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2))
    distance = vectorDist (position hero) (Vector2 (fromIntegral x) (fromIntegral y))
    ratio = distToProjPlane / distance
    
    projectedTop = round (fromIntegral halfActionHeight - (ratio * (wallHeight - heroHeight))) :: CInt
    projectedHeight = round (ratio * wallHeight) :: CInt
    actionY = projectedTop + (intRectY actionArea) :: CInt
    textureXDouble = hitWallTextureRatio * (fromIntegral textureWidth - 1)
    textureX = floor textureXDouble
    sourceRect = Just (mkSDLRect textureX 0 1 textureHeight)
    destRect = Just (mkSDLRect (pixel + actionAreaX) (actionY + actionAreaY) 1 projectedHeight)

--renderSprite :: SDL.Renderer -> RenderData -> (SDL.Texture, SDL.Rectangle CInt) -> Hero -> Vector2 -> Vector2 -> IO ()
--renderSprite r _ (texture, sourceRect) hero oPos oSize =
--  copyWithActionOffset r (intRectPos actionArea) texture sourceRect destRect
--  where
--    heroPos = position hero
--    heroLookAngle = rayAngle (heroLookRay hero)
--    itemAngle = vector2ToAngle (oPos - heroPos)
--    projectionAngle = itemAngle - heroLookAngle
--    fieldOfViewSize = pi / 3
--    angleRatio = (fieldOfViewSize - ((fieldOfViewSize / 2) + projectionAngle)) / fieldOfViewSize
--
--    distance = vectorDist heroPos oPos
--    -- TODO: Not sure of correct way to handle when 0 distance
--    ratio = distToProjPlane / max 0.01 distance
--    projectedTop = round (fromIntegral halfActionHeight - (ratio * (itemHeight - heroHeight)))
--    projectedHeight = round (ratio * itemHeight)
--    projectedWidth = ratio * v2x oSize
--    x = round ((fromIntegral actionWidth * angleRatio) - (projectedWidth / 2))
--    destRect = mkSDLRect x projectedTop (round projectedWidth) projectedHeight

renderWeapon :: SDL.Renderer -> RenderData -> WorldTicks -> Weapon -> IO ()
renderWeapon r RenderData {weaponTextures=wt} t w =
  copyWithActionOffset r (intRectPos actionArea) texture sourceRect destRect
  where
    totalAnimationTime = 400
    sinceUsed = fmap (t -) (lastTimeWeaponUsed w)
    animationTime = mfilter (< totalAnimationTime) sinceUsed
    progress = maybe 0 ((/ fromIntegral totalAnimationTime) . fromIntegral) animationTime
    animation = fromJust (M.lookup "Pistol" wt)
    texture = animationTexture animation
    sourceRect@(SDL.Rectangle _ (SDL.V2 tW tH)) = getAnimationFrame animation progress
    destX = actionAreaX + ((actionWidth - tW) `div` 2)
    destY = actionAreaY + (actionHeight - tH)
    destRect = mkSDLRect destX destY tW tH

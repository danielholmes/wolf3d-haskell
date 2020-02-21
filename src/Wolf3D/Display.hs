module Wolf3D.Display (
  setupRenderer,
  render,
  renderWorld,
  renderHud,
  rayLineIntersection,
  RenderData (RenderData),
  WallMaterialData,
  screenWidth,
  screenHeight,
  actionHeight,
  actionWidth
) where

import Wolf3D.Geom
import Wolf3D.Sim
import Wolf3D.Engine
import Wolf3D.Runner
import Wolf3D.SDLUtils
import Wolf3D.Display.Utils
import Wolf3D.Display.Hud
import Wolf3D.Animation
import qualified SDL
import Data.StateVar (($=))
import Data.Vector
import Data.Foldable
import Data.Maybe
import Control.Monad (mfilter)
import Data.Fixed
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
  renderItems r d w
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
renderWalls r d w = forM_ hits (renderWallLine r d)
  where hits = pixelWallHits w

renderWallLine :: SDL.Renderer -> RenderData -> (CInt, WallHit, Double) -> IO ()
renderWallLine r (RenderData {wallTextures=wt}) (x, WallHit (Wall o _ m) hit _, distance) = do
  copyWithActionOffset r (intRectPos actionArea) texture sourceRect destRect
  SDL.drawLine r from to
  where
    (texture, (textureWidth, textureHeight)) = fromJust (M.lookup m wt)
    hitWallX = vectorDist hit o
    hitWallTextureRatio = hitWallX `mod'` wallHeight / wallHeight
    ratio = distToProjPlane / distance
    projectedTop = round (fromIntegral halfActionHeight - (ratio * (wallHeight - heroHeight)))
    projectedHeight = round (ratio * wallHeight)
    from = SDL.P (SDL.V2 (x + (intRectX actionArea)) (projectedTop + (intRectY actionArea)))
    to = SDL.P (SDL.V2 x (projectedTop + projectedHeight))
    textureXDouble = hitWallTextureRatio * (fromIntegral textureWidth - 1)
    textureX = floor textureXDouble
    sourceRect = mkSDLRect textureX 0 1 textureHeight
    destRect = SDL.Rectangle from (SDL.V2 1 projectedHeight)

---- Solid colour
--renderWallLine :: SDL.Renderer -> RenderData -> (Int, WallHit, Double) -> IO ()
--renderWallLine r RenderData {halfSize=(_, halfHeight), distToProjPlane=d, wallTextures=wt} (x, WallHit (Wall _ _ m) _ _, distance) = do
--  SDL.rendererDrawColor r $= wallColour m
--  SDL.drawLine r from to
--  SDL.rendererDrawColor r $= SDL.V4 0 0 (fromIntegral (M.size wt)) darknessAlpha -- size only used so dont have to shuffle imports
--  SDL.drawLine r from to
--  where
--    ratio = d / distance
--    projectedTop = round (fromIntegral halfHeight - (ratio * (wallHeight - heroHeight)))
--    projectedHeight = round (ratio * wallHeight)
--    xInt = fromIntegral x
--    from = SDL.P (SDL.V2 xInt projectedTop)
--    to = SDL.P (SDL.V2 xInt (projectedTop + projectedHeight))
--    darknessMultiplier = 8000
--    intensity = 1 - min 1 ((1 / distance) * darknessMultiplier)
--    darknessAlpha = round (255 * intensity)

--wallColour :: WallMaterial -> SDL.V4 Word8
--wallColour Blue = SDL.V4 0 0 255 255
--wallColour Blue2 = SDL.V4 0 0 240 255
--wallColour Blue3 = SDL.V4 0 10 220 255
--wallColour Blue4 = SDL.V4 0 20 210 255
--wallColour Red = SDL.V4 255 0 0 255
--wallColour Green = SDL.V4 0 255 0 255

pixelWallHits :: World Wolf3DSimEntity -> [(CInt, WallHit, Double)]
pixelWallHits w = foldr foldStep [] hits
  where
    pixels = [0..(actionWidth - 1)]
    hits = map (\i -> (i, pixelWallHit w i)) pixels

    foldStep :: (CInt, Maybe (WallHit, Double)) -> [(CInt, WallHit, Double)] -> [(CInt, WallHit, Double)]
    foldStep (_, Nothing) accu = accu
    foldStep (i, Just (h, d)) accu = (i, h, d) : accu

pixelWallHit :: World Wolf3DSimEntity -> CInt -> Maybe (WallHit, Double)
pixelWallHit w i = fmap (\h -> (h, perpendicularDistance rayRotation h)) (castRayToClosestWall w rotatedRay)
  where
    hero = worldHero w
    hRay = heroLookRay hero
    ratio = fromIntegral (actionWidth - i) / fromIntegral actionWidth
    rayRotation = pi / 3 * (ratio - 0.5)
    rotatedRay = rotateRay hRay rayRotation

renderItems :: SDL.Renderer -> RenderData -> World Wolf3DSimEntity -> IO ()
renderItems r d w = forM_ (worldEnvItems w) (renderItem r d (worldHero w))

renderItem :: SDL.Renderer -> RenderData -> Hero -> EnvItem -> IO ()
renderItem r d@RenderData {itemTextures=it} hero i@(EnvItem t itemPos) =
  renderSprite r d texture hero itemPos (itemSize i)
  where
    texture = fromJust (M.lookup t it)

renderSprite :: SDL.Renderer -> RenderData -> (SDL.Texture, SDL.Rectangle CInt) -> Hero -> Vector2 -> Vector2 -> IO ()
renderSprite r _ (texture, sourceRect) hero oPos oSize =
  copyWithActionOffset r (intRectPos actionArea) texture sourceRect destRect
  --SDL.rendererDrawColor r $= SDL.V4 255 0 0 50
  --SDL.fillRect r (Just destRect)
  where
    heroPos = position hero
    heroLookAngle = rayAngle (heroLookRay hero)
    itemAngle = vector2ToAngle (oPos - heroPos)
    projectionAngle = itemAngle - heroLookAngle
    fieldOfViewSize = pi / 3
    angleRatio = (fieldOfViewSize - ((fieldOfViewSize / 2) + projectionAngle)) / fieldOfViewSize

    distance = vectorDist heroPos oPos
    -- TODO: Not sure of correct way to handle when 0 distance
    ratio = distToProjPlane / max 0.01 distance
    projectedTop = round (fromIntegral halfActionHeight - (ratio * (itemHeight - heroHeight)))
    projectedHeight = round (ratio * itemHeight)
    projectedWidth = ratio * v2x oSize
    x = round ((fromIntegral actionWidth * angleRatio) - (projectedWidth / 2))
    destRect = mkSDLRect x projectedTop (round projectedWidth) projectedHeight

perpendicularDistance :: Double -> WallHit -> Double
perpendicularDistance rayRotation (WallHit _ _ d) = d * cos rayRotation

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

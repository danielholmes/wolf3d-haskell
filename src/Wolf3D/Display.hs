module Wolf3D.Display (
  setupRenderer,
  render,
  renderWorld,
  renderHud,
  rayLineIntersection,
  RenderData (RenderData, areaSize)
) where

import Wolf3D.Geom
import Wolf3D.Sim
import Wolf3D.Engine
import Wolf3D.Runner
import Wolf3D.SDLUtils
import Wolf3D.Display.Utils
import Wolf3D.Animation
import qualified SDL
import Data.StateVar (($=))
import Data.Vector
import Data.Foldable
import Data.Maybe
import Control.Monad (mfilter)
--import Data.Word (Word8)
import Data.Fixed
import qualified Data.Map as M
import Foreign.C.Types (CInt)


type WallMaterialData = M.Map WallMaterial (SDL.Texture, (Int, Int))
type EnvItemData = M.Map EnvItemType (SDL.Texture, SDL.Rectangle CInt)
type WeaponData = M.Map String Animation
data RenderData = RenderData { areaSize :: (Int, Int)
                             , actionArea :: IntRectangle
                             , halfActionSize :: (Int, Int)
                             , distToProjPlane :: Double
                             , wallTextures :: WallMaterialData
                             , itemTextures :: EnvItemData
                             , weaponTextures :: WeaponData
                             , hudPlaceholder :: (SDL.Texture, SDL.Rectangle CInt)}

setupRenderer :: SDL.Renderer -> IO ()
setupRenderer r = SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend

render :: SDL.Renderer -> RenderData -> SimRun -> IO ()
render r d s = do
  renderHud r d
  renderWorld r d (simRunWorld s)
  SDL.present r

renderHud :: SDL.Renderer -> RenderData -> IO ()
renderHud r (RenderData {areaSize=(width, height), hudPlaceholder=(texture, sourceRect)}) = do
  SDL.copy r texture (Just sourceRect) (Just destRect)
  where
    destRect = SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 (fromIntegral width) (fromIntegral height))

renderWorld :: SDL.Renderer -> RenderData -> World Wolf3DSimEntity -> IO ()
renderWorld r d w = do
  renderCeilingAndFloor r d
  renderWalls r d w
  renderItems r d w
  renderWeapon r d (worldTime w) (worldHeroWeapon w)

renderCeilingAndFloor :: SDL.Renderer -> RenderData -> IO ()
renderCeilingAndFloor r RenderData {actionArea=(IntRectangle (x, y) (width, _)), halfActionSize=(_, halfH)} = do
  SDL.rendererDrawColor r $= SDL.V4 1 84 88 255
  SDL.fillRect r (Just (mkSDLRect (fromIntegral x) (fromIntegral y) cWidth cHalfH))
  SDL.rendererDrawColor r $= SDL.V4 112 112 112 255
  SDL.fillRect r (Just (mkSDLRect (fromIntegral x) cHalfH cWidth cHalfH))
  where
    cHalfH = fromIntegral halfH
    cWidth = fromIntegral width

renderWalls :: SDL.Renderer -> RenderData -> World Wolf3DSimEntity -> IO ()
renderWalls r d@RenderData {actionArea=(IntRectangle _ (width, _))} w = forM_ hits (renderWallLine r d)
  where hits = pixelWallHits w width

renderWallLine :: SDL.Renderer -> RenderData -> (Int, WallHit, Double) -> IO ()
renderWallLine r (RenderData {actionArea=(IntRectangle a@(areaX, areaY) _), halfActionSize=(_, halfHeight), distToProjPlane=d, wallTextures=wt}) (x, WallHit (Wall o _ m) hit _, distance) = do
  copyWithActionOffset r a texture sourceRect destRect
  SDL.rendererDrawColor r $= SDL.V4 0 0 0 darknessAlpha
  SDL.drawLine r from to
  where
    (texture, (textureWidth, textureHeight)) = fromJust (M.lookup m wt)
    hitWallX = vectorDist hit o
    hitWallTextureRatio = hitWallX `mod'` wallHeight / wallHeight
    ratio = d / distance
    projectedTop = round (fromIntegral halfHeight - (ratio * (wallHeight - heroHeight)))
    projectedHeight = round (ratio * wallHeight)
    from = SDL.P (SDL.V2 (fromIntegral (x + areaX)) (fromIntegral (projectedTop + (fromIntegral areaY))))
    to = SDL.P (SDL.V2 (fromIntegral x) (projectedTop + projectedHeight))
    darknessMultiplier = 8000
    intensity = 1 - min 1 ((1 / distance) * darknessMultiplier)
    darknessAlpha = round (255 * intensity)
    textureXDouble = hitWallTextureRatio * (fromIntegral textureWidth - 1)
    textureX = floor textureXDouble
    sourceRect = mkSDLRect textureX 0 1 (fromIntegral textureHeight)
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

pixelWallHits :: World Wolf3DSimEntity -> Int -> [(Int, WallHit, Double)]
pixelWallHits w width = foldr foldStep [] hits
  where
    pixels = [0..(width - 1)]
    hits = map (\i -> (i, pixelWallHit w width i)) pixels
    foldStep :: (Int, Maybe (WallHit, Double)) -> [(Int, WallHit, Double)] -> [(Int, WallHit, Double)]
    foldStep (_, Nothing) accu = accu
    foldStep (i, Just (h, d)) accu = (i, h, d) : accu

pixelWallHit :: World Wolf3DSimEntity -> Int -> Int -> Maybe (WallHit, Double)
pixelWallHit w width i = fmap (\h -> (h, perpendicularDistance rayRotation h)) (castRayToClosestWall w rotatedRay)
  where
    hero = worldHero w
    hRay = heroLookRay hero
    widthI = fromIntegral width
    ratio = fromIntegral i / widthI
    rayRotation = heroFieldOfViewSize hero * (ratio - 0.5)
    rotatedRay = rotateRay hRay rayRotation

renderItems :: SDL.Renderer -> RenderData -> World Wolf3DSimEntity -> IO ()
renderItems r d w = forM_ (worldEnvItems w) (renderItem r d (worldHero w))

renderItem :: SDL.Renderer -> RenderData -> Hero -> EnvItem -> IO ()
renderItem r d@RenderData {itemTextures=it} hero i@(EnvItem t itemPos) =
  renderSprite r d texture hero itemPos (itemSize i)
  where
    texture = fromJust (M.lookup t it)

renderSprite :: SDL.Renderer -> RenderData -> (SDL.Texture, SDL.Rectangle CInt) -> Hero -> Vector2 -> Vector2 -> IO ()
renderSprite r RenderData {actionArea=(IntRectangle a (width, _)), halfActionSize=(_, halfHeight), distToProjPlane=d} (texture, sourceRect) hero oPos oSize =
  copyWithActionOffset r a texture sourceRect destRect
  --SDL.rendererDrawColor r $= SDL.V4 255 0 0 50
  --SDL.fillRect r (Just destRect)
  where
    heroPos = heroPosition hero
    heroLookAngle = rayAngle (heroLookRay hero)
    itemAngle = vector2ToAngle (oPos - heroPos)
    projectionAngle = itemAngle - heroLookAngle
    fieldOfViewSize = heroFieldOfViewSize hero
    angleRatio = ((fieldOfViewSize / 2) + projectionAngle) / fieldOfViewSize

    distance = vectorDist heroPos oPos
    -- TODO: Not sure of correct way to handle when 0 distance
    ratio = d / max 0.01 distance
    projectedTop = round (fromIntegral halfHeight - (ratio * (itemHeight - heroHeight)))
    projectedHeight = round (ratio * itemHeight)
    projectedWidth = ratio * v2x oSize
    x = round ((fromIntegral width * angleRatio) - (projectedWidth / 2))
    destRect = mkSDLRect x projectedTop (round projectedWidth) projectedHeight

perpendicularDistance :: Double -> WallHit -> Double
perpendicularDistance rayRotation (WallHit _ _ d) = d * cos rayRotation

renderWeapon :: SDL.Renderer -> RenderData -> WorldTime -> Weapon -> IO ()
renderWeapon r RenderData {actionArea=(IntRectangle a (width, height)), weaponTextures=wt} t w =
  copyWithActionOffset r a texture sourceRect destRect
  where
    totalAnimationTime = 400
    sinceUsed = fmap (t -) (lastTimeWeaponUsed w)
    animationTime = mfilter (< totalAnimationTime) sinceUsed
    progress = maybe 0 ((/ fromIntegral totalAnimationTime) . fromIntegral) animationTime
    animation = fromJust (M.lookup "Pistol" wt)
    texture = animationTexture animation
    sourceRect@(SDL.Rectangle _ (SDL.V2 tW tH)) = getAnimationFrame animation progress
    destRect = mkSDLRect (fromIntegral (width - fromIntegral tW) `div` 2) (fromIntegral (height - fromIntegral tH)) tW tH

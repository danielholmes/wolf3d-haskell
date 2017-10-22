module Wolf3D.Display (
  setupRenderer,
  render,
  renderWorld,
  rayLineIntersection,
  RenderData (RenderData)
) where

import Wolf3D.Geom
import Wolf3D.Hero
import Wolf3D.Wolf3DSim
import Wolf3D.Sim
import Wolf3D.Runner
import Wolf3D.Environment
import Wolf3D.SDLUtils
import qualified SDL
import Data.StateVar (($=))
import Data.Vector
import Data.Foldable
import Data.Maybe
import Data.Fixed (mod')
import qualified Data.Map as M
import Foreign.C.Types (CInt)


type WallMaterialData = M.Map WallMaterial (SDL.Texture, (Int, Int))
type ItemTypeData = M.Map EnvItemType (SDL.Texture, SDL.Rectangle CInt)
type WeaponData = M.Map String (SDL.Texture, SDL.Rectangle CInt)
data RenderData = RenderData { size :: (Int, Int)
                             , halfSize :: (Int, Int)
                             , distToProjPlane :: Double
                             , wallTextures :: WallMaterialData
                             , itemTextures :: ItemTypeData
                             , weaponTextures :: WeaponData}

setupRenderer :: SDL.Renderer -> IO ()
setupRenderer r = SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend

render :: SDL.Renderer -> RenderData -> SimRun -> IO ()
render r d s = do
  renderWorld r d (simRunWorld s)
  SDL.present r

renderWorld :: SDL.Renderer -> RenderData -> World Wolf3DSimItem -> IO ()
renderWorld r d w = do
  renderCeilingAndFloor r d
  renderWalls r d w
  renderItems r d w
  renderWeapon r d (worldHeroWeapon w)

renderCeilingAndFloor :: SDL.Renderer -> RenderData -> IO ()
renderCeilingAndFloor r RenderData {size=(width, _), halfSize=(_, halfH)} = do
  SDL.rendererDrawColor r $= SDL.V4 1 84 88 255
  SDL.fillRect r (Just (mkSDLRect 0 0 cWidth cHalfH))
  SDL.rendererDrawColor r $= SDL.V4 112 112 112 255
  SDL.fillRect r (Just (mkSDLRect 0 cHalfH cWidth cHalfH))
  where
    cHalfH = fromIntegral halfH
    cWidth = fromIntegral width

renderWalls :: SDL.Renderer -> RenderData -> World Wolf3DSimItem -> IO ()
renderWalls r d@RenderData {size=(width, _)} w = forM_ hits (renderWallLine r d)
  where hits = pixelWallHits w width

renderWallLine :: SDL.Renderer -> RenderData -> (Int, WallHit, Double) -> IO ()
renderWallLine r RenderData {halfSize=(_, halfHeight), distToProjPlane=d, wallTextures=wt} (x, WallHit (Wall o _ m) hit _, distance) = do
  SDL.copy r texture (Just sourceRect) (Just destRect)
  SDL.rendererDrawColor r $= SDL.V4 0 0 0 alpha
  SDL.drawLine r from to
  where
    ratio = d / distance
    projectedTop = round (fromIntegral halfHeight - (ratio * (wallHeight - heroHeight)))
    projectedHeight = round (ratio * wallHeight)
    xInt = fromIntegral x
    (texture, (textureWidth, textureHeight)) = fromJust (M.lookup m wt)
    hitWallX = vectorDist hit o
    hitWallTextureRatio = hitWallX `mod'` wallHeight / wallHeight
    textureXDouble = hitWallTextureRatio * (fromIntegral textureWidth - 1)
    textureX = floor textureXDouble
    from = SDL.P (SDL.V2 xInt projectedTop)
    to = SDL.P (SDL.V2 xInt (projectedTop + projectedHeight))
    darknessMultiplier = 8000
    intensity = 1 - min 1 ((1 / distance) * darknessMultiplier)
    alpha = round (255 * intensity)
    sourceRect = mkSDLRect textureX 0 1 (fromIntegral textureHeight)
    destRect = mkSDLRect xInt projectedTop 1 projectedHeight

-- Solid color
--renderWallLine :: SDL.Renderer -> RenderData -> (Int, WallHit, Double) -> IO ()
--renderWallLine r (RenderData _ s) (x, hit, distance) = do
--  SDL.rendererDrawColor r $= wallHitColour hit
--  SDL.drawLine r from to
--  where
--    heroHeight = 1500
--    wallHeight = 3000
--    distanceToProjectionPlane = (v2x s / 2) / tan30
--    ratio = distanceToProjectionPlane / distance
--    halfScreenHeight = v2y s / 2
--    projectedTop = round (halfScreenHeight - (ratio * (wallHeight - heroHeight)))
--    projectedHeight = round (ratio * wallHeight)
--    xInt = CInt (fromIntegral x)
--    from = SDL.P (SDL.V2 xInt projectedTop)
--    to = SDL.P (SDL.V2 xInt (projectedTop + projectedHeight))

pixelWallHits :: World Wolf3DSimItem -> Int -> [(Int, WallHit, Double)]
pixelWallHits w width = foldr foldStep [] hits
  where
    pixels = [0..(width - 1)]
    hits = map (\i -> (i, pixelWallHit w width i)) pixels
    foldStep :: (Int, Maybe (WallHit, Double)) -> [(Int, WallHit, Double)] -> [(Int, WallHit, Double)]
    foldStep (_, Nothing) accu = accu
    foldStep (i, Just (h, d)) accu = (i, h, d) : accu

pixelWallHit :: World Wolf3DSimItem -> Int -> Int -> Maybe (WallHit, Double)
pixelWallHit w width i = fmap (\h -> (h, perpendicularDistance rayRotation h)) (castRayToClosestWall w rotatedRay)
  where
    hero = worldHero w
    hRay = heroLookRay hero
    widthI = fromIntegral width
    ratio = fromIntegral i / widthI
    rayRotation = heroFieldOfViewSize hero * (ratio - 0.5)
    rotatedRay = rotateRay hRay rayRotation

renderItems :: SDL.Renderer -> RenderData -> World Wolf3DSimItem -> IO ()
renderItems r d w = forM_ (worldEnvItems w) (renderItem r d (worldHero w))

renderItem :: SDL.Renderer -> RenderData -> Hero -> EnvItem -> IO ()
renderItem r d@RenderData {itemTextures=it} hero i@(EnvItem t itemPos) =
  renderSprite r d texture hero itemPos (itemSize i)
  where
    texture = fromJust (M.lookup t it)

renderSprite :: SDL.Renderer -> RenderData -> (SDL.Texture, SDL.Rectangle CInt) -> Hero -> Vector2 -> Vector2 -> IO ()
renderSprite r RenderData {size=(width, _), halfSize=(_, halfHeight), distToProjPlane=d} (texture, sourceRect) hero oPos oSize =
  SDL.copy r texture (Just sourceRect) (Just destRect)
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

renderWeapon :: SDL.Renderer -> RenderData -> Weapon -> IO ()
renderWeapon r RenderData {size=(width, height), weaponTextures=wt} _ =
  SDL.copy r texture (Just sourceRect) (Just destRect)
  where
    (texture, sourceRect@(SDL.Rectangle _ (SDL.V2 tW tH))) = fromJust (M.lookup "Pistol" wt)
    destRect = mkSDLRect (fromIntegral (width - fromIntegral tW) `div` 2) (fromIntegral (height - fromIntegral tH)) tW tH


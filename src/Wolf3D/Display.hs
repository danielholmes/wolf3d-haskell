module Wolf3D.Display (
  setupRenderer,
  render,
  renderWorld,
  rayLineIntersection,
  RenderData (RenderData)
) where

import Wolf3D.Types
import Wolf3D.Geom
import Wolf3D.Hero
import Wolf3D.World
import Wolf3D.SDLUtils
import qualified SDL
import Data.StateVar (($=))
import Data.Vector
import Data.Foldable
import Data.Word
import Data.Map (Map)
import Foreign.C.Types (CInt (CInt))


data RenderData = RenderData (Map WallMaterial (SDL.Texture, (PosInt, PosInt))) Vector2

setupRenderer :: SDL.Renderer -> IO ()
setupRenderer _ = return ()

render :: SDL.Renderer -> RenderData -> World -> IO ()
render r d w = do
  renderWorld r d w
  SDL.present r

renderWorld :: SDL.Renderer -> RenderData -> World -> IO ()
renderWorld r d@(RenderData _ s) w = do
  renderCeilingAndFloor r s
  renderWalls r d w

renderCeilingAndFloor :: SDL.Renderer -> Vector2 -> IO ()
renderCeilingAndFloor r s = do
  let width = round (v2x s)
  let halfHeight = round (v2y s / 2)
  SDL.rendererDrawColor r $= SDL.V4 1 84 88 255
  SDL.fillRect r (Just (mkSDLRect 0 0 width halfHeight))
  SDL.rendererDrawColor r $= SDL.V4 112 112 112 255
  SDL.fillRect r (Just (mkSDLRect 0 halfHeight width halfHeight))

renderWalls :: SDL.Renderer -> RenderData -> World -> IO ()
renderWalls r d@(RenderData _ s) w = forM_ hits (renderWallLine r d)
  where hits = pixelWallHits s w (posInt (round (v2x s)))

tan30 :: Double
tan30 = tan (pi / 6)

renderWallLine :: SDL.Renderer -> RenderData -> (PosZInt, WallHit, PosZDouble) -> IO ()
renderWallLine r (RenderData _ s) (x, hit, distance) = do
  SDL.rendererDrawColor r $= wallHitColour hit
  SDL.drawLine r from to
  where
    heroHeight = 1500
    wallHeight = 3000
    distanceToProjectionPlane = (v2x s / 2) / tan30
    ratio = distanceToProjectionPlane / fromPosZDouble distance
    halfScreenHeight = v2y s / 2
    projectedTop = round (halfScreenHeight - (ratio * (wallHeight - heroHeight)))
    projectedHeight = round (ratio * wallHeight)
    xInt = CInt (fromIntegral (fromPosZInt x))
    from = SDL.P (SDL.V2 xInt projectedTop)
    to = SDL.P (SDL.V2 xInt (projectedTop + projectedHeight))

wallHitColour :: WallHit -> SDL.V4 Word8
wallHitColour (WallHit (Wall _ _ material) _ distance) = SDL.V4 red green blue 255
  where
    maxScaler = 30000.0
    distanceRatio = 1.0 - min 1.0 (fromPosZDouble distance / maxScaler)
    colour = fromIntegral (round (255.0 * distanceRatio) :: Int)
    (red, green, blue) = case material of
      Red -> (colour, 0, 0)
      Green -> (0, colour, 0)
      Blue -> (0, 0, colour)
      Blue2 -> (0, 0, colour)
      Blue3 -> (0, 0, colour)
      Blue4 -> (0, 0, colour)

pixelWallHits :: Vector2 -> World -> PosInt -> [(PosZInt, WallHit, PosZDouble)]
pixelWallHits _ w width = foldr foldStep [] hits
  where
    pixels = map posZInt [0..(fromPosInt width - 1)]
    hits = map (\i -> (i, pixelWallHit w width i)) pixels
    foldStep :: (PosZInt, Maybe (WallHit, PosZDouble)) -> [(PosZInt, WallHit, PosZDouble)] -> [(PosZInt, WallHit, PosZDouble)]
    foldStep (_, Nothing) accu = accu
    foldStep (i, Just (h, d)) accu = (i, h, d) : accu

pixelWallHit :: World -> PosInt -> PosZInt -> Maybe (WallHit, PosZDouble)
pixelWallHit w width i = fmap (\h -> (h, perpendicularDistance rayRotation h)) (castRayToClosestWall w rotatedRay)
  where
    hRay = heroLookRay (worldHero w)
    widthI = fromIntegral (fromPosInt width)
    ratio = fromIntegral (fromPosZInt i) / widthI
    rayRotation = fieldOfView * (ratio - 0.5)
    rotatedRay = rotateRay hRay rayRotation

perpendicularDistance :: Double -> WallHit -> PosZDouble
perpendicularDistance rayRotation (WallHit _ _ d) = posZDouble (fromPosZDouble d * cos rayRotation)

fieldOfView :: Double
fieldOfView = pi / 3

module Wolf3D.Display (
  setupRenderer,
  render,
  renderWorld,
  castRayToClosestWall,
  rayLineIntersection
) where

import Wolf3D.Types
import Wolf3D.Geom
import Wolf3D.Hero
import Wolf3D.World
import Wolf3D.SDLUtils
import qualified SDL
import Data.StateVar (($=))
import Data.List
import Data.Vector
import Data.Foldable
import Data.Word
import Foreign.C.Types (CInt (CInt))


type WallHit = (PosZDouble, Wall, Vector2)

setupRenderer :: SDL.Renderer -> IO ()
setupRenderer _ = return ()

render :: SDL.Renderer -> Vector2 -> World -> IO ()
render r s w = do
  renderWorld r s w
  SDL.present r

renderWorld :: SDL.Renderer -> Vector2 -> World -> IO ()
renderWorld r s w = do
  renderCeilingAndFloor r s
  renderWalls r s w

renderCeilingAndFloor :: SDL.Renderer -> Vector2 -> IO ()
renderCeilingAndFloor r s = do
  let width = round (v2x s)
  let halfHeight = round (v2y s / 2)
  SDL.rendererDrawColor r $= SDL.V4 1 84 88 255
  SDL.fillRect r (Just (mkSDLRect 0 0 width halfHeight))
  SDL.rendererDrawColor r $= SDL.V4 112 112 112 255
  SDL.fillRect r (Just (mkSDLRect 0 halfHeight width halfHeight))

renderWalls :: SDL.Renderer -> Vector2 -> World -> IO ()
renderWalls r s w = forM_ hits (renderWallLine r s)
  where hits = visibleWallLines w (posInt (round (v2x s)))

renderWallLine :: SDL.Renderer -> Vector2 -> (PosZInt, Maybe WallHit) -> IO ()
renderWallLine _ _ (_, Nothing) = return ()
renderWallLine r s (x, Just hit@(distance, _, _)) = do
  let xInt = CInt (fromIntegral (fromPosZInt x))
  let maxScaler = 40000.0
  let distanceRatio = max 0 (1.0 - (fromPosZDouble distance / maxScaler))
  let height = round (300 * distanceRatio)
  let halfHeight = height `div` 2
  let halfScreenHeight = round (v2y s) `div` 2
  SDL.rendererDrawColor r $= wallHitColour hit
  SDL.drawLine r (SDL.P (SDL.V2 xInt (halfScreenHeight - halfHeight))) (SDL.P (SDL.V2 xInt (halfScreenHeight + halfHeight)))

wallHitColour :: WallHit -> SDL.V4 Word8
wallHitColour (distance, Wall _ _ material, _) = SDL.V4 red green blue 255
  where
    maxScaler = 30000.0
    distanceRatio = 1.0 - min 1.0 (fromPosZDouble distance / maxScaler)
    colour = fromIntegral (round (255.0 * distanceRatio) :: Int)
    (red, green, blue) = case material of
      Red -> (colour, 0, 0)
      Green -> (0, colour, 0)
      Blue -> (0, 0, colour)

visibleWallLines :: World -> PosInt -> [(PosZInt, Maybe WallHit)]
visibleWallLines w width = map (\i -> (posZInt i, castRayToClosestWall w (wallVisionRay (worldHero w) i width))) [0..(fromPosInt width - 1)]

wallVisionRay :: Hero -> Int -> PosInt -> Ray
wallVisionRay hero i width = moveRayAlongDirection rotatedRay (-focalLength)
  where
    focalLength = 30
    hRay = heroLookRay hero
    widthI = fromIntegral (fromPosInt width)
    ratio = fromIntegral i / widthI - 0.5
    rayRotation = 0.9 * ratio
    rotatedRay = rotateRay hRay rayRotation

castRayToClosestWall :: World -> Ray -> Maybe WallHit
castRayToClosestWall w ray
  | null allHits = Nothing
  | otherwise    = Just (minimumBy compareHits allHits)
  where
    allHits = castRay w ray
    compareHits :: WallHit -> WallHit -> Ordering
    compareHits (d1, _, _) (d2, _, _) = d1 `compare` d2

castRay :: World -> Ray -> [WallHit]
castRay world ray = foldr foldStep [] (worldWalls world)
  where
    rStart = rayOrigin ray
    foldStep :: Wall -> [WallHit] -> [WallHit]
    foldStep wall accu = case rayLineIntersection ray (wallToLine wall) of
      Nothing -> accu
      Just foundPos -> (vectorDist rStart foundPos, wall, foundPos) : accu

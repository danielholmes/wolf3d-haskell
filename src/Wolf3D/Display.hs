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

render :: SDL.Renderer -> World -> IO ()
render r w = do
  renderWorld r w
  SDL.present r

renderWorld :: SDL.Renderer -> World -> IO ()
renderWorld r w = do
  renderCeilingAndFloor r
  renderWalls r w

renderCeilingAndFloor :: SDL.Renderer -> IO ()
renderCeilingAndFloor r = do
  SDL.rendererDrawColor r $= SDL.V4 1 84 88 255
  SDL.fillRect r (Just (mkSDLRect 0 0 640 240))
  SDL.rendererDrawColor r $= SDL.V4 112 112 112 255
  SDL.fillRect r (Just (mkSDLRect 0 240 640 240))

renderWalls :: SDL.Renderer -> World -> IO ()
renderWalls r w = forM_ hits (renderWallLine r)
  where hits = visibleWallLines w (posInt 640)

renderWallLine :: SDL.Renderer -> (PosZInt, Maybe WallHit) -> IO ()
renderWallLine _ (_, Nothing) = return ()
renderWallLine r (x, Just hit@(distance, _, _)) = do
  let xInt = CInt (fromIntegral (fromPosZInt x))
  let maxScaler = 40000.0
  let distanceRatio = 1.0 - min 1.0 (fromPosZDouble distance / maxScaler)
  let height = round (200 * distanceRatio)
  let halfHeight = height `div` 2
  SDL.rendererDrawColor r $= wallHitColour hit
  SDL.drawLine r (SDL.P (SDL.V2 xInt (480 `div` 2 - halfHeight))) (SDL.P (SDL.V2 xInt (480 `div` 2 + halfHeight)))

wallHitColour :: WallHit -> SDL.V4 Word8
wallHitColour (distance, Wall _ _ material, _) = SDL.V4 red green blue 255
  where
    maxScaler = 40000.0
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
    rayRotation = 1.8 * ratio
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

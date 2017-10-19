module Wolf3D.Display (render, Ray, castRayToClosestWall, rayLineIntersection) where

import Wolf3D.Types
import Wolf3D.Geom
import Wolf3D.Hero
import Wolf3D.World
import qualified SDL
import Data.StateVar
import Data.List
import Data.Vector
import Data.Foldable
import Data.Word
import Foreign.C.Types


type Line = (Vector2, Vector2)
type WallHit = (PosZDouble, Wall, Vector2)

render :: SDL.Texture -> SDL.Renderer -> World -> IO ()
render t r w = do
  renderCeilingAndFloor r
  renderWalls r w

  -- Dummy character rendering
  let pos = worldHeroPosition w
  SDL.copy r t (Just (mkRect 0 0 48 48)) (Just (mkRect (round (v2x pos)) (round (v2y pos)) 48 48))

  SDL.present r

renderCeilingAndFloor :: SDL.Renderer -> IO ()
renderCeilingAndFloor r = do
  SDL.rendererDrawColor r $= SDL.V4 1 84 88 255
  SDL.fillRect r (Just (mkRect 0 0 640 240))
  SDL.rendererDrawColor r $= SDL.V4 112 112 112 255
  SDL.fillRect r (Just (mkRect 0 240 640 240))

renderWalls :: SDL.Renderer -> World -> IO ()
renderWalls r w = forM_ hits (renderWallLine r)
  where hits = visibleWallLines w (posInt 640)

renderWallLine :: SDL.Renderer -> (PosZInt, Maybe WallHit) -> IO ()
renderWallLine _ (_, Nothing) = return ()
renderWallLine r (x, Just hit@(distance, _, _)) = do
  let xInt = CInt (fromIntegral (fromPosZInt x))
  let maxScaler = 2000.0
  let distanceRatio = 1.0 - min 1.0 (fromPosZDouble distance / maxScaler)
  let height = round (200 * distanceRatio)
  let halfHeight = height `div` 2
  SDL.rendererDrawColor r $= wallHitColour hit
  SDL.drawLine r (SDL.P (SDL.V2 xInt (480 `div` 2 - halfHeight))) (SDL.P (SDL.V2 xInt (480 `div` 2 + halfHeight)))

wallHitColour :: WallHit -> SDL.V4 Word8
wallHitColour (distance, Wall _ _ material, _) = SDL.V4 red green blue 255
  where
    maxScaler = 2000.0
    distanceRatio = 1.0 - min 1.0 (fromPosZDouble distance / maxScaler)
    colour = fromIntegral (round (255.0 * distanceRatio) :: Int)
    (red, green, blue) = case material of
      Red -> (colour, 0, 0)
      Green -> (0, colour, 0)
      Blue -> (0, 0, colour)

visibleWallLines :: World -> PosInt -> [(PosZInt, Maybe WallHit)]
visibleWallLines w width = map (\i -> (posZInt i, castRayToClosestWall w (wallVisionRay (worldHero w) i width))) [0..(fromPosInt width - 1)]

wallVisionRay :: Hero -> Int -> PosInt -> Ray
wallVisionRay hero i width = createRay (pos - focalMagnitude) focalMagnitude
  where
    focalMagnitude = Vector2 rayX focalLength
    pos = heroPosition hero
    widthI = fromIntegral (fromPosInt width)
    halfWidth = widthI / 2.0
    focalLength = 30
    ratio = fromIntegral i / widthI
    rayX = 0.1 * (ratio * widthI - halfWidth)

wallToLine :: Wall -> Line
wallToLine (Wall start change _) = (start, change)

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

vectorDist :: Vector2 -> Vector2 -> PosZDouble
vectorDist (Vector2 x1 y1) (Vector2 x2 y2) = posZDouble (sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2)))

-- https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect/565282#565282
-- https://rootllama.wordpress.com/2014/06/20/ray-line-segment-intersection-test-in-2d/
rayLineIntersection :: Ray -> Line -> Maybe Vector2
rayLineIntersection ray (q, s)
  | rxs == 0 && qmpxr == 0                 = Nothing -- Collinear
  | rxs == 0 && qmpxr /= 0                 = Nothing -- Parallel
  | rxs /= 0 && t >= 0 && u >= 0 && u <= 1 = Just intersectP
  | otherwise                              = Nothing
  where
    p = rayOrigin ray
    r = rayDirection ray
    rxs = vcross2 r s
    qmp = q - p
    qmpxr = vcross2 qmp r
    u = vcross2 qmp r / rxs
    t = vcross2 qmp s / rxs
    intersectP = p + (t |* r)

vcross2 :: Vector2 -> Vector2 -> Scalar
vcross2 (Vector2 x1 y1) (Vector2 x2 y2) = (x1 * y2) - (y1 * x2)

mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h

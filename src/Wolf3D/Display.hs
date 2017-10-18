module Wolf3D.Display (render, Ray, castRayToClosestWall, rayLineIntersection) where

import Wolf3D.Types
import Wolf3D.World
import qualified SDL
import Data.StateVar
import Data.List
import Data.Vector
import Data.Foldable
import Foreign.C.Types


type Ray = (Vector2, Vector2)
type Line = (Vector2, Vector2)
type WallHit = (PosZDouble, Wall, Vector2)

render :: SDL.Texture -> SDL.Renderer -> World -> IO ()
render t r w = do
  let (x,y) = worldPosition w
  SDL.rendererDrawColor r $= SDL.V4 255 255 255 255
  SDL.clear r

  let seenLines = visibleLines w (posInt 640)
  renderLines r seenLines

  SDL.copy r t (Just (mkRect 0 0 48 48)) (Just (mkRect (fromIntegral x) (fromIntegral y) 48 48))

  SDL.present r

renderLines :: SDL.Renderer -> [(PosZInt, Maybe WallHit)] -> IO ()
renderLines r hits = forM_ hits (renderLine r)


renderLine :: SDL.Renderer -> (PosZInt, Maybe WallHit) -> IO ()
renderLine _ (_, Nothing) = return ()
renderLine r (x, Just (distance, _, _)) = do
  let xInt = CInt (fromIntegral (fromPosZInt x))
  let distanceSeed = round (fromPosZDouble distance / 1000) :: Int
  let distanceWord = fromIntegral distanceSeed
  SDL.rendererDrawColor r $= SDL.V4 distanceWord 0 0 255
  SDL.drawLine r (SDL.P (SDL.V2 xInt 100)) (SDL.P (SDL.V2 xInt 300))

visibleLines :: World -> PosInt -> [(PosZInt, Maybe WallHit)]
visibleLines _ width = map (\i -> (posZInt i, Nothing)) [1..(fromPosInt width)]

wallToLine :: Wall -> Line
wallToLine (Wall (x, y) (dx, dy)) = (Vector2 (fromIntegral x) (fromIntegral y), Vector2 (fromIntegral dx) (fromIntegral dy))

castRayToClosestWall :: World -> Ray -> Maybe WallHit
castRayToClosestWall w ray
  | null allHits = Nothing
  | otherwise    = Just (minimumBy compareHits allHits)
  where
    allHits = castRay w ray
    compareHits :: WallHit -> WallHit -> Ordering
    compareHits (d1, _, _) (d2, _, _) = d1 `compare` d2

castRay :: World -> Ray -> [WallHit]
castRay world ray@(rStart, _) = foldr foldStep [] (worldWalls world)
  where
    foldStep :: Wall -> [WallHit] -> [WallHit]
    foldStep wall accu = case rayLineIntersection ray (wallToLine wall) of
      Nothing -> accu
      Just foundPos -> (vectorDist rStart foundPos, wall, foundPos) : accu

vectorDist :: Vector2 -> Vector2 -> PosZDouble
vectorDist (Vector2 x1 y1) (Vector2 x2 y2) = posZDouble (sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2)))

-- https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect/565282#565282
-- https://rootllama.wordpress.com/2014/06/20/ray-line-segment-intersection-test-in-2d/
rayLineIntersection :: Ray -> Line -> Maybe Vector2
rayLineIntersection (p, r) (q, s)
  | rxs == 0 && qmpxr == 0                 = Nothing -- Collinear
  | rxs == 0 && qmpxr /= 0                 = Nothing -- Parallel
  | rxs /= 0 && t >= 0 && u >= 0 && u <= 1 = Just intersectP
  | otherwise                              = Nothing
  where
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

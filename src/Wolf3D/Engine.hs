{-# LANGUAGE GADTs #-}
module Wolf3D.Engine (
  SimEntity (simUpdate),
  World,
  WorldTicks,
  Wall (Wall),
  WallMaterial (Red, Green, Blue, Blue2, Blue3, Blue4),
  WallHit (WallHit),
  TileCoord,
  Ceiling (GreyCeiling, GreenCeiling, PurpleCeiling, YellowCeiling),
  createWorld,
  worldWalls,
  worldEntities,
  updateWorldEntities,
  ticWorldTicks,
  worldWallsTouching,
  wallToLine,
  castRayToClosestWall,
  wallHeight,
  worldTics,
  worldCeilingColor,
  tickWorld,
  tickWorldNTimes
) where

import Wolf3D.Geom
import Data.Vector
import Data.List


class SimEntity i where
  simUpdate :: World a -> i -> i

-- TODO: See if way of moving wall materials outside of engine
data WallMaterial = Red | Green | Blue | Blue2 | Blue3 | Blue4
  deriving (Show, Eq, Ord)

type TileCoord = (Int, Int)
type WallPosition = Vector2
type WallSize = Vector2
data Wall = Wall WallPosition WallSize WallMaterial
  deriving (Show, Eq)

type DistanceToWall = Double
type HitPosition = Vector2
data WallHit = WallHit Wall HitPosition DistanceToWall
  deriving (Show, Eq)

data Ceiling = GreyCeiling | PurpleCeiling | GreenCeiling | YellowCeiling
  deriving (Show, Eq, Ord)

type WorldTicks = Int
-- Tried record syntax for this and failed
data World i where
  World :: (SimEntity i) => Ceiling -> [Wall] -> [i] -> WorldTicks -> World i

createWorld :: (SimEntity i) => Ceiling -> [Wall] -> [i] -> World i
createWorld c walls items = World c walls items 0

tickWorld :: World i -> World i
tickWorld world@(World _ _ is _) = ticWorldTicks updatedWorld
  where
    updatedItems = map (simUpdate world) is
    updatedWorld = updateWorldEntities world updatedItems

tickWorldNTimes :: World i -> Int -> Maybe (World i)
tickWorldNTimes w n
  | n == 0    = Nothing
  | otherwise = Just (foldr foldStep w [1..n])
    where
      foldStep :: Int -> World i -> World i
      foldStep _ = tickWorld

updateWorldEntities :: World i -> [i] -> World i
updateWorldEntities (World c w _ t) i = World c w i t

worldWalls :: World i -> [Wall]
worldWalls (World _ walls _ _) = walls

worldEntities :: (SimEntity i) => World i -> [i]
worldEntities (World _ _ is _) = is

worldCeilingColor :: World i -> Ceiling
worldCeilingColor (World c _ _ _) = c

worldTics :: World i -> Int
worldTics (World _ _ _ t) = t

worldWallsTouching :: World i -> Rectangle -> [Wall]
worldWallsTouching w r = filter (wallIsTouching r) (worldWalls w)

wallIsTouching :: Rectangle -> Wall -> Bool
wallIsTouching r w = rectangleTouchesLine r (wallToLine w)

wallToLine :: Wall -> Line
wallToLine (Wall start change _) = (start, change)

wallHeight :: Double
wallHeight = 3000

ticWorldTicks :: World i -> World i
ticWorldTicks (World c ws is tics) = World c ws is (tics + 1)

castRayToClosestWall :: World i -> Ray -> Maybe WallHit
castRayToClosestWall w ray
  | null allHits = Nothing
  | otherwise    = Just (minimumBy compareHits allHits)
  where
    allHits = castRayToAllWalls w ray
    compareHits :: WallHit -> WallHit -> Ordering
    compareHits (WallHit _ _ d1) (WallHit _ _ d2) = d1 `compare` d2

castRayToAllWalls :: World i -> Ray -> [WallHit]
castRayToAllWalls world ray = foldr foldStep [] (worldWalls world)
  where
    rStart = rayOrigin ray
    foldStep :: Wall -> [WallHit] -> [WallHit]
    foldStep wall accu = case rayLineIntersection ray (wallToLine wall) of
      Nothing -> accu
      Just pos -> WallHit wall pos (vectorDist rStart pos) : accu

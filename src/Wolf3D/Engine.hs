{-# LANGUAGE GADTs #-}
module Wolf3D.Engine (
  SimEntity (simUpdate),
  StepMillis,
  World,
  WorldTime,
  Wall (Wall),
  WallMaterial (Red, Green, Blue, Blue2, Blue3, Blue4),
  WallHit (WallHit),
  createWorld,
  worldWalls,
  worldEntities,
  updateWorldEntities,
  advanceWorldTime,
  worldWallsTouching,
  wallToLine,
  castRayToClosestWall,
  wallHeight,
  worldTime,
  tickWorld,
  tickWorldNTimes
) where

import Wolf3D.Geom
import Data.Vector
import Data.List


type StepMillis = Int
class SimEntity i where
  simUpdate :: World a -> StepMillis -> i -> i

-- TODO: See if way of moving wall materials outside of engine
data WallMaterial = Red | Green | Blue | Blue2 | Blue3 | Blue4
  deriving (Show, Eq, Ord)

type WallPosition = Vector2
type WallSize = Vector2
data Wall = Wall WallPosition WallSize WallMaterial
  deriving (Show, Eq)

type DistanceToWall = Double
type HitPosition = Vector2
data WallHit = WallHit Wall HitPosition DistanceToWall
  deriving (Show, Eq)

type WorldTime = Int
data World i where
  World :: (SimEntity i) => [Wall] -> [i] -> WorldTime -> World i

createWorld :: (SimEntity i) => [Wall] -> [i] -> World i
createWorld walls items = World walls items 0

tickWorld :: Int -> World i -> World i
tickWorld timeStep world@(World _ is _) = advanceWorldTime updatedWorld timeStep
  where
    updatedItems = map (simUpdate world timeStep) is
    updatedWorld = updateWorldEntities world updatedItems

tickWorldNTimes :: World i -> Int -> Int -> Maybe (World i)
tickWorldNTimes w f n
  | n == 0    = Nothing
  | otherwise = Just (foldr foldStep w [1..n])
    where
      foldStep :: Int -> World i -> World i
      foldStep _ = tickWorld f

updateWorldEntities :: World i -> [i] -> World i
updateWorldEntities (World w _ t) i = World w i t

worldWalls :: World i -> [Wall]
worldWalls (World walls _ _) = walls

worldEntities :: (SimEntity i) => World i -> [i]
worldEntities (World _ is _) = is

worldTime :: World i -> Int
worldTime (World _ _ t) = t

worldWallsTouching :: World i -> Rectangle -> [Wall]
worldWallsTouching w r = filter (wallIsTouching r) (worldWalls w)

wallIsTouching :: Rectangle -> Wall -> Bool
wallIsTouching r w = rectangleTouchesLine r (wallToLine w)

wallToLine :: Wall -> Line
wallToLine (Wall start change _) = (start, change)

wallHeight :: Double
wallHeight = 3000

advanceWorldTime :: World i -> Int -> World i
advanceWorldTime (World ws is time) step = World ws is (time + step)

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

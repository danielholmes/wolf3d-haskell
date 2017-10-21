module Wolf3D.World (
  World,
  Wall (Wall),
  WallMaterial (Red, Green, Blue, Blue2, Blue3, Blue4),
  WallHit (WallHit),
  createWorld,
  worldHeroPosition,
  worldWalls,
  worldHero,
  worldItems,
  updateWorldHeroActionsState,
  advanceWorldTime,
  updateWorldHero,
  worldWallsTouching,
  worldItemsTouching,
  wallToLine,
  castRayToClosestWall,
  wallHeight
) where

import Wolf3D.Geom
import Wolf3D.Hero
import Wolf3D.Items
import Data.Vector
import Data.List


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

type WorldTimeMillis = Int
data World = World Hero [Wall] [Item] WorldTimeMillis

createWorld :: [Wall] -> [Item] -> World
createWorld walls items = World hero walls items 0
  where hero = createHero (Vector2 0 0)

worldHeroPosition :: World -> Vector2
worldHeroPosition (World h _ _ _) = heroPosition h

worldWalls :: World -> [Wall]
worldWalls (World _ walls _ _) = walls

worldItems :: World -> [Item]
worldItems (World _ _ is _) = is

worldItemsTouching :: World -> Rectangle -> [Item]
worldItemsTouching w r = filter (itemIsTouching r) (worldItems w)

itemIsTouching :: Rectangle -> Item -> Bool
itemIsTouching r i = rectangleOverlapsRectangle r (itemRectangle i)

worldWallsTouching :: World -> Rectangle -> [Wall]
worldWallsTouching w r = filter (wallIsTouching r) (worldWalls w)

wallIsTouching :: Rectangle -> Wall -> Bool
wallIsTouching r w = rectangleTouchesLine r (wallToLine w)

wallToLine :: Wall -> Line
wallToLine (Wall start change _) = (start, change)

wallHeight :: Double
wallHeight = 3000

worldHero :: World -> Hero
worldHero (World h _ _ _) = h

updateWorldHeroActionsState :: World -> HeroActionsState -> World
updateWorldHeroActionsState w@(World h _ _ _) a = updateWorldHero w (updateHeroActionsState h a)

advanceWorldTime :: World -> Int -> World
advanceWorldTime (World p ws is time) step = World p ws is (time + step)

updateWorldHero :: World -> Hero -> World
updateWorldHero (World _ ws is time) h = World h ws is time

castRayToClosestWall :: World -> Ray -> Maybe WallHit
castRayToClosestWall w ray
  | null allHits = Nothing
  | otherwise    = Just (minimumBy compareHits allHits)
  where
    allHits = castRayToAllWalls w ray
    compareHits :: WallHit -> WallHit -> Ordering
    compareHits (WallHit _ _ d1) (WallHit _ _ d2) = d1 `compare` d2

castRayToAllWalls :: World -> Ray -> [WallHit]
castRayToAllWalls world ray = foldr foldStep [] (worldWalls world)
  where
    rStart = rayOrigin ray
    foldStep :: Wall -> [WallHit] -> [WallHit]
    foldStep wall accu = case rayLineIntersection ray (wallToLine wall) of
      Nothing -> accu
      Just pos -> WallHit wall pos (vectorDist rStart pos) : accu

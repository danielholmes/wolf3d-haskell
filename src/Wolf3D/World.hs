module Wolf3D.World (
  World,
  Wall (Wall),
  WallMaterial (Red, Green, Blue, Blue2, Blue3, Blue4),
  WallHit (WallHit),
  createWorld,
  worldPlayerActionsState,
  worldHeroPosition,
  worldWalls,
  worldHero,
  updateWorldPlayerActionsState,
  advanceWorldTime,
  updateWorldHero,
  worldWallsTouching,
  wallToLine,
  castRayToClosestWall
) where

import Wolf3D.Hero
import Wolf3D.Geom
import Wolf3D.Player
import Wolf3D.Types
import Data.Vector
import Data.List


data WallMaterial = Red | Green | Blue | Blue2 | Blue3 | Blue4

type WallPosition = Vector2
type WallSize = Vector2
data Wall = Wall WallPosition WallSize WallMaterial

instance Show WallMaterial where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"
  show Blue2  = "Blue2"
  show Blue3  = "Blue3"
  show Blue4  = "Blue4"

instance Ord WallMaterial where
  m1 `compare` m2 = show m1 `compare` show m2

instance Eq WallMaterial where
  (==) Red Red = True
  (==) Green Green = True
  (==) Blue Blue = True
  (==) Blue2 Blue2 = True
  (==) Blue3 Blue3 = True
  (==) Blue4 Blue4 = True
  (==) _ _ = False

instance Show Wall where
  show (Wall s e m) = "Wall " ++ show s ++ " " ++ show e ++ " " ++ show m

instance Eq Wall where
  (==) (Wall s1 e1 m1) (Wall s2 e2 m2) = s1 == s2 && e1 == e2 && m1 == m2

type DistanceToWall = PosZDouble
type HitPosition = Vector2
data WallHit = WallHit Wall HitPosition DistanceToWall

instance Show WallHit where
  show (WallHit wall pos distance) = "WallHit " ++ show wall ++ " " ++ show pos ++ " " ++ show distance

instance Eq WallHit where
  (==) (WallHit w1 p1 d1) (WallHit w2 p2 d2) = w1 == w2 && p1 == p2 && d1 == d2

type WorldTimeMillis = PosZInt
data World = World Hero [Wall] PlayerActionsState WorldTimeMillis

createWorld :: [Wall] -> World
createWorld walls = World hero walls staticPlayerActionsState posZInt0
  where
    hero = createHero (Vector2 0 0)

worldPlayerActionsState :: World -> PlayerActionsState
worldPlayerActionsState (World _ _ a _) = a

worldHeroPosition :: World -> Vector2
worldHeroPosition (World h _ _ _) = heroPosition h

worldWalls :: World -> [Wall]
worldWalls (World _ walls _ _) = walls

worldWallsTouching :: World -> Rectangle -> [Wall]
worldWallsTouching w r = filter (wallIsTouching r) (worldWalls w)

wallIsTouching :: Rectangle -> Wall -> Bool
wallIsTouching r w = rectangleTouchesLine r (wallToLine w)

wallToLine :: Wall -> Line
wallToLine (Wall start change _) = (start, change)

worldHero :: World -> Hero
worldHero (World h _ _ _) = h

updateWorldPlayerActionsState :: World -> PlayerActionsState -> World
updateWorldPlayerActionsState (World p w _ r) s = World p w s r

advanceWorldTime :: World -> PosInt -> World
advanceWorldTime (World p ws pas time) step = World p ws pas newTime
  where
    newTime = posZInt (fromPosZInt time + fromPosInt step)

updateWorldHero :: World -> Hero -> World
updateWorldHero (World _ ws pas time) h = World h ws pas time

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

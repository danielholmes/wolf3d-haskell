module Wolf3D.World (
  World,
  Wall (Wall),
  WallMaterial (Red, Green, Blue),
  createWorld,
  worldPlayerActionsState,
  worldHeroPosition,
  worldWalls,
  worldHero,
  updateWorldPlayerActionsState,
  advanceWorldTime,
  updateWorldHero,
  worldWallsTouching,
  wallToLine
) where

import Wolf3D.Hero
import Wolf3D.Geom
import Wolf3D.Player
import Wolf3D.Types
import Data.Vector


data WallMaterial = Red | Green | Blue
type WallPosition = Vector2
data Wall = Wall WallPosition WallPosition WallMaterial

instance Show WallMaterial where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"

instance Eq WallMaterial where
  (==) Red Red = True
  (==) Green Green = True
  (==) Blue Blue = True
  (==) _ _ = False

instance Show Wall where
  show (Wall s e m) = "Wall " ++ show s ++ " " ++ show e ++ " " ++ show m

instance Eq Wall where
  (==) (Wall s1 e1 m1) (Wall s2 e2 m2) = s1 == s2 && e1 == e2 && m1 == m2


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

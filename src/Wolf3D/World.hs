module Wolf3D.World (
  World,
  Wall (Wall),
  createWorld,
  worldPlayerActionsState,
  worldPosition,
  worldWalls,
  updateWorldPlayerActionsState,
  advanceWorldTime,
  moveWorld
) where

import Wolf3D.Player
import Wolf3D.Types


type WallPosition = (Int, Int)
data Wall = Wall WallPosition WallPosition

instance Show Wall where
  show (Wall s e) = "Wall " ++ show s ++ " " ++ show e

instance Eq Wall where
  (==) (Wall s1 e1) (Wall s2 e2) = s1 == s2 && e1 == e2


type WorldTimeMillis = PosZInt
data World = World (Int, Int) [Wall] PlayerActionsState WorldTimeMillis

createWorld :: [Wall] -> World
createWorld walls = World (0,0) walls staticPlayerActionsState posZInt0

worldPlayerActionsState :: World -> PlayerActionsState
worldPlayerActionsState (World _ _ a _) = a

worldPosition :: World -> (Int, Int)
worldPosition (World p _ _ _) = p

worldWalls :: World -> [Wall]
worldWalls (World _ walls _ _) = walls

updateWorldPlayerActionsState :: World -> PlayerActionsState -> World
updateWorldPlayerActionsState (World p w _ r) s = World p w s r

advanceWorldTime :: World -> PosInt -> World
advanceWorldTime (World p ws pas time) step = World p ws pas newTime
  where
    newTime = posZInt (fromPosZInt time + fromPosInt step)

-- TODO: Remove
moveWorld :: World -> (Int,Int) -> World
moveWorld (World (x,y) ws pas time) (dx,dy) = World (x + dx, y + dy) ws pas time

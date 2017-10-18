module Wolf3D.World (
  World,
  initWorld,
  worldPlayerActionsState,
  worldPosition,
  updateWorldPlayerActionsState,
  advanceWorldTime,
  moveWorld
) where

import Wolf3D.Player
import Wolf3D.Types

type WorldTimeMillis = PosZInt
data World = World (Int, Int) PlayerActionsState WorldTimeMillis

initWorld :: World
initWorld = World (0,0) staticPlayerActionsState posZInt0

worldPlayerActionsState :: World -> PlayerActionsState
worldPlayerActionsState (World _ a _) = a

worldPosition :: World -> (Int, Int)
worldPosition (World p _ _) = p

updateWorldPlayerActionsState :: World -> PlayerActionsState -> World
updateWorldPlayerActionsState (World p _ r) s = World p s r

advanceWorldTime :: World -> PosInt -> World
advanceWorldTime (World p pas time) step = World p pas newTime
  where
    newTime = posZInt (fromPosZInt time + fromPosInt step)

-- TODO: Remove
moveWorld :: World -> (Int,Int) -> World
moveWorld (World (x,y) pas time) (dx,dy) = World (x + dx, y + dy) pas time

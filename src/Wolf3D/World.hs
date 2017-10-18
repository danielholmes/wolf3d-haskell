module Wolf3D.World (
  PositionWorld (PositionWorld),
  initWorld,
  worldPlayerActionsState,
  updateWorldPlayerActionsState
) where

import Wolf3D.Player
import Wolf3D.Types

type WorldTimeMillis = PosZInt
data PositionWorld = PositionWorld (Int, Int) PlayerActionsState WorldTimeMillis

initWorld :: PositionWorld
initWorld = PositionWorld (0,0) staticPlayerActionsState posZInt0

worldPlayerActionsState :: PositionWorld -> PlayerActionsState
worldPlayerActionsState (PositionWorld _ a _) = a

updateWorldPlayerActionsState :: PositionWorld -> PlayerActionsState -> PositionWorld
updateWorldPlayerActionsState (PositionWorld p _ r) s = PositionWorld p s r

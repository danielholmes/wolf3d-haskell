module Wolf3D.World (
  PositionWorld (PositionWorld),
  initWorld,
  worldPlayerActionsState,
  updateWorldPlayerActionsState
) where

import Wolf3D.Player

type Rotation = Int
data PositionWorld = PositionWorld (Int, Int) PlayerActionsState Rotation

initWorld :: PositionWorld
initWorld = PositionWorld (0,0) staticPlayerActionsState 0

worldPlayerActionsState :: PositionWorld -> PlayerActionsState
worldPlayerActionsState (PositionWorld _ a _) = a

updateWorldPlayerActionsState :: PositionWorld -> PlayerActionsState -> PositionWorld
updateWorldPlayerActionsState (PositionWorld p _ r) s = PositionWorld p s r

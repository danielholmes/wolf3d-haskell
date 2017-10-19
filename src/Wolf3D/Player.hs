module Wolf3D.Player (
  PlayerActionsState,
  staticPlayerActionsState,
  modifyPlayerActionState,
  PlayerAction (MoveForward, MoveBackward, TurnLeft, TurnRight),
  playerActionsStateMoveForward,
  playerActionsStateMoveBackward,
  playerActionsStateTurnLeft,
  playerActionsStateTurnRight
) where

data PlayerActionsState = PlayerActionsState
  { playerActionsStateMoveForward  :: Bool
  , playerActionsStateMoveBackward :: Bool
  , playerActionsStateTurnLeft     :: Bool
  , playerActionsStateTurnRight    :: Bool
  }

data PlayerAction = MoveForward | MoveBackward | TurnLeft | TurnRight

staticPlayerActionsState :: PlayerActionsState
staticPlayerActionsState = PlayerActionsState False False False False

modifyPlayerActionState :: PlayerActionsState -> PlayerAction -> Bool -> PlayerActionsState
modifyPlayerActionState (PlayerActionsState _ d l r) MoveForward a = PlayerActionsState a d l r
modifyPlayerActionState (PlayerActionsState u _ l r) MoveBackward a = PlayerActionsState u a l r
modifyPlayerActionState (PlayerActionsState u d _ r) TurnLeft a = PlayerActionsState u d a r
modifyPlayerActionState (PlayerActionsState u d l _) TurnRight a = PlayerActionsState u d l a

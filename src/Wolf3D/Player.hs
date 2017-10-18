module Wolf3D.Player (
  PlayerActionsState,
  staticPlayerActionsState,
  modifyPlayerActionState,
  PlayerAction (MoveUp, MoveDown, TurnLeft, TurnRight),
  playerActionsStateUp,
  playerActionsStateDown,
  playerActionsStateTurnLeft,
  playerActionsStateTurnRight
) where

data PlayerActionsState = PlayerActionsState
  { playerActionsStateUp    :: Bool
  , playerActionsStateDown  :: Bool
  , playerActionsStateTurnLeft  :: Bool
  , playerActionsStateTurnRight :: Bool
  }

data PlayerAction = MoveUp | MoveDown | TurnLeft | TurnRight

staticPlayerActionsState :: PlayerActionsState
staticPlayerActionsState = PlayerActionsState False False False False

modifyPlayerActionState :: PlayerActionsState -> PlayerAction -> Bool -> PlayerActionsState
modifyPlayerActionState (PlayerActionsState _ d l r) MoveUp a = PlayerActionsState a d l r
modifyPlayerActionState (PlayerActionsState u _ l r) MoveDown a = PlayerActionsState u a l r
modifyPlayerActionState (PlayerActionsState u d _ r) TurnLeft a = PlayerActionsState u d a r
modifyPlayerActionState (PlayerActionsState u d l _) TurnRight a = PlayerActionsState u d l a

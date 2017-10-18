module Wolf3D.Player (
  PlayerActionsState,
  staticPlayerActionsState,
  modifyPlayerActionState,
  PlayerAction (MoveUp, MoveDown, MoveLeft, MoveRight),
  playerActionsStateUp,
  playerActionsStateDown,
  playerActionsStateLeft,
  playerActionsStateRight
) where

data PlayerActionsState = PlayerActionsState
  { playerActionsStateUp    :: Bool
  , playerActionsStateDown  :: Bool
  , playerActionsStateLeft  :: Bool
  , playerActionsStateRight :: Bool
  }

data PlayerAction = MoveUp | MoveDown | MoveLeft | MoveRight

staticPlayerActionsState :: PlayerActionsState
staticPlayerActionsState = PlayerActionsState False False False False

modifyPlayerActionState :: PlayerActionsState -> PlayerAction -> Bool -> PlayerActionsState
modifyPlayerActionState (PlayerActionsState _ d l r) MoveUp a = PlayerActionsState a d l r
modifyPlayerActionState (PlayerActionsState u _ l r) MoveDown a = PlayerActionsState u a l r
modifyPlayerActionState (PlayerActionsState u d _ r) MoveLeft a = PlayerActionsState u d a r
modifyPlayerActionState (PlayerActionsState u d l _) MoveRight a = PlayerActionsState u d l a

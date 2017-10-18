module Wolf3D.Input (processInput, InputState, inputQuit, inputPlayerActionsState) where

import Wolf3D.Player
import SDL

data InputState = Quit | Running PlayerActionsState

inputQuit :: InputState -> Bool
inputQuit Quit = True
inputQuit _ = False

inputPlayerActionsState :: InputState -> PlayerActionsState
inputPlayerActionsState (Running a) = a
inputPlayerActionsState _ = error "Invalid"

processInput :: PlayerActionsState -> IO InputState
processInput p = do
  event <- pollEvent
  case event of
    Just (Event _ QuitEvent) -> return Quit
    Just (Event _ (KeyboardEvent (KeyboardEventData _ Released False keySym))) -> return (Running (processKeyRelease p keySym))
    Just (Event _ (KeyboardEvent (KeyboardEventData _ Pressed False keySym))) -> return (Running (processKeyPress p keySym))
    _ -> return (Running p)

processKeyPress :: PlayerActionsState -> Keysym -> PlayerActionsState
processKeyPress = processKeyAction True

processKeyRelease :: PlayerActionsState -> Keysym -> PlayerActionsState
processKeyRelease = processKeyAction False

processKeyAction :: Bool -> PlayerActionsState -> Keysym -> PlayerActionsState
processKeyAction active p keySym = case keysymKeycode keySym of
  KeycodeUp     -> modifyPlayerActionState p MoveUp active
  KeycodeDown   -> modifyPlayerActionState p MoveDown active
  KeycodeLeft   -> modifyPlayerActionState p MoveLeft active
  KeycodeRight  -> modifyPlayerActionState p MoveRight active
  _             -> p


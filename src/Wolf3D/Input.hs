module Wolf3D.Input (processInput, InputState, inputQuit, inputHeroActionsState) where

import Wolf3D.Hero
import SDL

data InputState = Quit | Running HeroActionsState

inputQuit :: InputState -> Bool
inputQuit Quit = True
inputQuit _ = False

inputHeroActionsState :: InputState -> HeroActionsState
inputHeroActionsState (Running a) = a
inputHeroActionsState _ = error "Invalid"

processInput :: HeroActionsState -> IO InputState
processInput p = do
  event <- pollEvent
  case event of
    Just (Event _ QuitEvent) -> return Quit
    Just (Event _ (KeyboardEvent (KeyboardEventData _ Released False keySym))) -> return (Running (processKeyRelease p keySym))
    Just (Event _ (KeyboardEvent (KeyboardEventData _ Pressed False keySym))) -> return (Running (processKeyPress p keySym))
    _ -> return (Running p)

processKeyPress :: HeroActionsState -> Keysym -> HeroActionsState
processKeyPress = processKeyAction True

processKeyRelease :: HeroActionsState -> Keysym -> HeroActionsState
processKeyRelease = processKeyAction False

processKeyAction :: Bool -> HeroActionsState -> Keysym -> HeroActionsState
processKeyAction active p keySym = case keysymKeycode keySym of
  KeycodeUp     -> modifyHeroActionState p MoveForward active
  KeycodeDown   -> modifyHeroActionState p MoveBackward active
  KeycodeLeft   -> modifyHeroActionState p TurnLeft active
  KeycodeRight  -> modifyHeroActionState p TurnRight active
  KeycodeLCtrl  -> modifyHeroActionState p Shoot active
  _             -> p


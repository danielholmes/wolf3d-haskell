module Wolf3D.Input (processInput) where

import Wolf3D.World
import SDL

processInput :: PositionWorld -> IO (Maybe PositionWorld)
processInput w = do
  event <- pollEvent
  case event of
    Just (Event _ QuitEvent) -> return Nothing
    Just (Event _ (KeyboardEvent (KeyboardEventData _ Released False keySym))) -> return (Just (processKeyRelease w keySym))
    Just (Event _ (KeyboardEvent (KeyboardEventData _ Pressed False keySym))) -> return (Just (processKeyPress w keySym))
    _ -> return (Just w)

processKeyPress :: PositionWorld -> Keysym -> PositionWorld
processKeyPress w@(PositionWorld pos (dx,dy) t) keySym = case keysymKeycode keySym of
  KeycodeUp     -> PositionWorld pos (dx,dy-1) t
  KeycodeDown   -> PositionWorld pos (dx,dy+1) t
  KeycodeLeft   -> PositionWorld pos (dx-1,dy) t
  KeycodeRight  -> PositionWorld pos (dx+1,dy) t
  _             -> w

processKeyRelease :: PositionWorld -> Keysym -> PositionWorld
processKeyRelease w@(PositionWorld pos (dx,dy) t) keySym = case keysymKeycode keySym of
  KeycodeUp     -> PositionWorld pos (dx,dy+1) t
  KeycodeDown   -> PositionWorld pos (dx,dy-1) t
  KeycodeLeft   -> PositionWorld pos (dx+1,dy) t
  KeycodeRight  -> PositionWorld pos (dx-1,dy) t
  _             -> w

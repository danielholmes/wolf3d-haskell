module Wolf3D.Input (processInput) where

import Wolf3D.World
import SDL

processInput :: PositionWorld -> IO PositionWorld
processInput w@(PositionWorld pos d _) = do
  event <- pollEvent
  case event of
    Just (Event _ QuitEvent) -> return (PositionWorld pos d True)
    Just (Event _ (KeyboardEvent (KeyboardEventData _ Released False keySym))) -> return (processKeyRelease w keySym)
    Just (Event _ (KeyboardEvent (KeyboardEventData _ Pressed False keySym))) -> return (processKeyPress w keySym)
    _ -> return w

processKeyPress :: PositionWorld -> Keysym -> PositionWorld
processKeyPress w@(PositionWorld pos (dx,dy) q) keySym = case keysymKeycode keySym of
  KeycodeUp     -> PositionWorld pos (dx,dy-1) q
  KeycodeDown   -> PositionWorld pos (dx,dy+1) q
  KeycodeLeft   -> PositionWorld pos (dx-1,dy) q
  KeycodeRight  -> PositionWorld pos (dx+1,dy) q
  _                 -> w

processKeyRelease :: PositionWorld -> Keysym -> PositionWorld
processKeyRelease w@(PositionWorld pos (dx,dy) q) keySym = case keysymKeycode keySym of
  KeycodeUp     -> PositionWorld pos (dx,dy+1) q
  KeycodeDown   -> PositionWorld pos (dx,dy-1) q
  KeycodeLeft   -> PositionWorld pos (dx+1,dy) q
  KeycodeRight  -> PositionWorld pos (dx-1,dy) q
  _                 -> w

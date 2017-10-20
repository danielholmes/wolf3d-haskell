module Wolf3D.SDLUtils (
  toSDLRect,
  mkSDLRect,
  mkOriginSDLRect,
  roundToSDLP
) where

import qualified SDL
import qualified SDL.Vect
import Foreign.C.Types (CInt)
import Wolf3D.Geom
import Data.Vector


mkSDLRect :: a -> a -> a -> a -> SDL.Rectangle a
mkSDLRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h

mkOriginSDLRect :: Vector2 -> SDL.Rectangle CInt
mkOriginSDLRect (Vector2 w h) = mkSDLRect 0 0 (round w) (round h)

toSDLRect :: Rectangle -> SDL.Rectangle CInt
toSDLRect (Rectangle (Vector2 x y) (Vector2 w h)) = mkSDLRect (round x) (round y) (round w) (round h)

roundToSDLP :: Vector2 -> SDL.Vect.Point SDL.Vect.V2 CInt
roundToSDLP (Vector2 x y) = SDL.Vect.P (SDL.Vect.V2 (round x) (round y))

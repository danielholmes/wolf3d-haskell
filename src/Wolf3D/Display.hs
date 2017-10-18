module Wolf3D.Display (render) where

import Wolf3D.World
import qualified SDL
import Data.StateVar

render :: SDL.Texture -> SDL.Renderer -> PositionWorld -> IO ()
render t r (PositionWorld (x,y) _ _) = do
  SDL.rendererDrawColor r $= SDL.V4 255 255 255 255
  SDL.clear r

  SDL.rendererDrawColor r $= SDL.V4 255 0 0 0
  SDL.drawLine r (SDL.P (SDL.V2 0 100)) (SDL.P (SDL.V2 400 100))
  SDL.copy r t (Just (mkRect 0 0 48 48)) (Just (mkRect (fromIntegral x) (fromIntegral y) 48 48))

  SDL.present r

mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h

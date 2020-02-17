module Wolf3D.Display.Utils (
  withViewport,
  copyWithActionOffset
) where

import qualified SDL
import Data.StateVar (($=), get)
import Foreign.C.Types (CInt)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)


withViewport :: (MonadIO m) => SDL.Renderer -> Maybe (SDL.Rectangle CInt) -> m a -> m ()
withViewport r vp op = do
  oldViewport <- get (SDL.rendererViewport r)
  SDL.rendererViewport r $= vp
  void op
  SDL.rendererViewport r $= oldViewport

copyWithActionOffset :: (MonadIO m) => SDL.Renderer -> (Int, Int) -> SDL.Texture -> SDL.Rectangle CInt -> SDL.Rectangle CInt -> m ()
copyWithActionOffset r _ texture sourceRect destRect = do
  SDL.copy r texture (Just sourceRect) (Just destRect)

module Wolf3D.Display.Utils (
  withViewport,
  withScale,
  copyWithActionOffset
) where

import qualified SDL
import Data.StateVar (($=), get)
import Foreign.C.Types (CInt, CFloat)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)


withViewport :: (MonadIO m) => SDL.Renderer -> Maybe (SDL.Rectangle CInt) -> m a -> m ()
withViewport r vp op = do
  oldViewport <- get (SDL.rendererViewport r)
  SDL.rendererViewport r $= vp
  void op
  SDL.rendererViewport r $= oldViewport

withScale :: (MonadIO m) => SDL.Renderer -> SDL.V2 CFloat -> m a -> m ()
withScale r s op = do
  oldScale <- get (SDL.rendererScale r)
  SDL.rendererScale r $= s
  void op
  SDL.rendererScale r $= oldScale

-- TODO: Not impl yet
copyWithActionOffset :: (MonadIO m) => SDL.Renderer -> (CInt, CInt) -> SDL.Texture -> SDL.Rectangle CInt -> SDL.Rectangle CInt -> m ()
copyWithActionOffset r _ texture sourceRect destRect = do
  SDL.copy r texture (Just sourceRect) (Just destRect)

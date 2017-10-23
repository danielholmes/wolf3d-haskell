{-# LANGUAGE OverloadedStrings #-}
module Wolf3D.UI (createUI) where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)


createUI :: (MonadIO m) => (Int, Int) -> (SDL.Renderer -> m a) -> m ()
createUI (width, height) op = do
  SDL.initialize []
  SDL.Image.initialize []
  SDL.Font.initialize
  void $
    withWindow "Wolfenstein 3D" (width, height) $ \w ->
      withRenderer w op
  SDL.Font.quit
  SDL.Image.quit
  SDL.quit

withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
  w <- SDL.createWindow title p
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w
    where
      p = SDL.defaultWindow { SDL.windowInitialSize = z }
      z = SDL.V2 (fromIntegral x) (fromIntegral y)

withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
  r <- SDL.createRenderer w (-1) rendererConfig
  void $ op r
  SDL.destroyRenderer r

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }

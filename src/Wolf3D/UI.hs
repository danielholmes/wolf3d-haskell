{-# LANGUAGE OverloadedStrings #-}
module Wolf3D.UI (createUI) where

import qualified SDL
import qualified SDL.Image
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Vector
import Wolf3D.Types


createUI :: (MonadIO m) => (SDL.Renderer -> Vector2 -> m a) -> m ()
createUI op = do
  SDL.initialize []
  SDL.Image.initialize []
  let width = 640
  let height = 480
  void $
    withWindow "Wolfenstein 3D" (posInt width, posInt height) $ \w ->
      withRenderer w (\r -> op r (Vector2 (fromIntegral width) (fromIntegral height)))
  SDL.Image.quit
  SDL.quit

withWindow :: (MonadIO m) => Text -> (PosInt, PosInt) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
  w <- SDL.createWindow title p
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w
    where
      p = SDL.defaultWindow { SDL.windowInitialSize = z }
      z = SDL.V2 (fromIntegral (fromPosInt x)) (fromIntegral (fromPosInt y))

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

--loadTextureWithInfo :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
--loadTextureWithInfo r p = do
--  t <- SDL.Image.loadTexture r p
--  i <- SDL.queryTexture t
--  pure (t, i)


--mkPoint :: a -> a -> SDL.Point SDL.V2 a
--mkPoint x y = SDL.P (SDL.V2 x y)

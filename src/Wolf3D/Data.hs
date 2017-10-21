module Wolf3D.Data (loadRenderData) where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Video.Renderer
import Wolf3D.Display
import Wolf3D.World
import Wolf3D.Items
import Data.Map (Map, fromList)
import Data.Vector


tan30 :: Double
tan30 = tan (pi / 6)

loadRenderData :: SDL.Renderer -> Vector2 -> IO RenderData
loadRenderData r s = do
  w <- loadWallTextures r
  i <- loadItemTextures r
  return (RenderData s ((v2x s / 2) / tan30) w i)

loadWallTextures :: SDL.Renderer -> IO (Map WallMaterial (SDL.Texture, (Int, Int)))
loadWallTextures r = do
  blue <- loadTexture r "blue.png"
  blue2 <- loadTexture r "blue2.png"
  blue3 <- loadTexture r "blue3.png"
  blue4 <- loadTexture r "blue4.png"
  red <- loadTexture r "blue.png"
  green <- loadTexture r "blue2.png"
  return (fromList [ (Blue, blue)
                   , (Blue2, blue2)
                   , (Blue3, blue3)
                   , (Blue4, blue4)
                   , (Red, red)
                   , (Green, green)])

loadItemTextures :: SDL.Renderer -> IO (Map ItemType (SDL.Texture, (Int, Int)))
loadItemTextures r = do
  drum <- loadTexture r "drum.png"
  flag <- loadTexture r "flag.png"
  light <- loadTexture r "light.png"
  return (fromList [ (Drum, drum), (Flag, flag), (Light, light)])

loadTexture :: SDL.Renderer -> FilePath -> IO (SDL.Texture, (Int, Int))
loadTexture r p = do
  t <- SDL.Image.loadTexture r ("assets/" ++ p)
  i <- SDL.queryTexture t
  let w = fromIntegral (SDL.Video.Renderer.textureWidth i)
  let h = fromIntegral (SDL.Video.Renderer.textureHeight i)
  return (t, (w, h))

module Wolf3D.Data (loadRenderData) where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Video.Renderer
import Wolf3D.Display
import Wolf3D.Sim
import Wolf3D.Environment
import Wolf3D.SDLUtils
import Data.Map (Map, fromList)
import Foreign.C.Types (CInt)


tan30 :: Double
tan30 = tan (pi / 6)

loadRenderData :: SDL.Renderer -> (Int, Int) -> IO RenderData
loadRenderData r s@(width, height) = do
  w <- loadWallTextures r
  i <- loadItemTextures r
  return (RenderData s (width `div` 2, height `div` 2) (fromIntegral (width `div` 2) / tan30) w i)

loadWallTextures :: SDL.Renderer -> IO (Map WallMaterial (SDL.Texture, (Int, Int)))
loadWallTextures r = do
  blue <- loadTexture r "blue.png"
  blue2 <- loadTexture r "blue2.png"
  blue3 <- loadTexture r "blue3.png"
  blue4 <- loadTexture r "blue4.png"
  red <- loadTexture r "blue.png"
  green <- loadTexture r "blue2.png"
  let entries = [ (Blue, blue)
                , (Blue2, blue2)
                , (Blue3, blue3)
                , (Blue4, blue4)
                , (Red, red)
                , (Green, green)]
  return (fromList (map rectToSize entries))
  where
    rectToSize :: (WallMaterial, (SDL.Texture, SDL.Rectangle CInt)) -> (WallMaterial, (SDL.Texture, (Int, Int)))
    rectToSize (m, (t, SDL.Rectangle _ (SDL.V2 width height))) = (m, (t, (fromIntegral width, fromIntegral height)))

loadItemTextures :: SDL.Renderer -> IO (Map EnvItemType (SDL.Texture, SDL.Rectangle CInt))
loadItemTextures r = do
  drum <- loadTexture r "drum.png"
  flag <- loadTexture r "flag.png"
  light <- loadTexture r "light.png"
  return (fromList [ (Drum, drum), (Flag, flag), (Light, light)])

loadTexture :: SDL.Renderer -> FilePath -> IO (SDL.Texture, SDL.Rectangle CInt)
loadTexture r p = do
  t <- SDL.Image.loadTexture r ("assets/" ++ p)
  i <- SDL.queryTexture t
  let w = fromIntegral (SDL.Video.Renderer.textureWidth i)
  let h = fromIntegral (SDL.Video.Renderer.textureHeight i)
  return (t, mkSDLRect 0 0 w h)

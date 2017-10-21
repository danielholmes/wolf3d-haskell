module Wolf3D.Data (loadRenderData) where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Video.Renderer
import Wolf3D.Types
import Wolf3D.Display
import Wolf3D.World
import Wolf3D.Items
import Data.Map (Map, fromList)
import Data.Vector
import Foreign.C.Types (CInt (CInt))


loadRenderData :: SDL.Renderer -> Vector2 -> IO RenderData
loadRenderData r s = do
  w <- loadWallTextures r
  i <- loadItemTextures r
  return (RenderData s w i)

loadWallTextures :: SDL.Renderer -> IO (Map WallMaterial (SDL.Texture, (PosInt, PosInt)))
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

loadItemTextures :: SDL.Renderer -> IO (Map ItemType (SDL.Texture, (PosInt, PosInt)))
loadItemTextures r = do
  drum <- loadTexture r "drum.png"
  return (fromList [ (Drum, drum)])

loadTexture :: SDL.Renderer -> FilePath -> IO (SDL.Texture, (PosInt, PosInt))
loadTexture r p = do
  t <- SDL.Image.loadTexture r ("assets/" ++ p)
  i <- SDL.queryTexture t
  let w = cIntToPosInt (SDL.Video.Renderer.textureWidth i)
  let h = cIntToPosInt (SDL.Video.Renderer.textureHeight i)
  return (t, (w, h))


cIntToPosInt :: CInt -> PosInt
cIntToPosInt (CInt i) = posInt (fromIntegral i)

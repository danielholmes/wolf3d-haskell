module Wolf3D.Data (loadRenderData) where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Video.Renderer
import Wolf3D.Types
import Wolf3D.Display
import Wolf3D.World
import Data.Map (Map, fromList)
import Data.Vector
import Foreign.C.Types (CInt (CInt))


loadRenderData :: SDL.Renderer -> Vector2 -> IO RenderData
loadRenderData r s = do
  w <- loadWallTextures r
  return (RenderData w s)

loadWallTextures :: SDL.Renderer -> IO (Map WallMaterial (SDL.Texture, (PosInt, PosInt)))
loadWallTextures r = do
  b <- loadWallTexture r Blue "blue.png"
  b2 <- loadWallTexture r Blue2 "blue2.png"
  return (fromList [b, b2])

loadWallTexture :: SDL.Renderer -> WallMaterial -> FilePath -> IO (WallMaterial, (SDL.Texture, (PosInt, PosInt)))
loadWallTexture r m p = do
  t <- SDL.Image.loadTexture r ("assets/" ++ p)
  i <- SDL.queryTexture t
  let w = cIntToPosInt (SDL.Video.Renderer.textureWidth i)
  let h = cIntToPosInt (SDL.Video.Renderer.textureHeight i)
  return (m, (t, (w, h)))

cIntToPosInt :: CInt -> PosInt
cIntToPosInt (CInt i) = posInt (fromIntegral i)

module Wolf3D.Loader (loadRenderData) where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Video.Renderer
import Wolf3D.Engine
import Wolf3D.Display
import Wolf3D.Sim
import Wolf3D.Animation
import Wolf3D.SDLUtils
import Data.Maybe
import Data.Map (Map, fromList)
import Foreign.C.Types (CInt)


loadRenderData :: SDL.Renderer -> IO RenderData
loadRenderData r = do
  w <- loadWallDatas r
  i <- loadEnvItemsData r
  weapons <- loadWeaponData r
  hudBase <- loadHudBaseData r
  bjFace <- loadBjFace r
  numbers <- loadNumbers r
  hudWeapons <- loadHudWeapons r
  return (RenderData w i weapons hudBase bjFace numbers hudWeapons)

loadHudBaseData :: SDL.Renderer -> IO (SDL.Texture, SDL.Rectangle CInt)
loadHudBaseData r = loadTexture r "hud-base.png"

loadBjFace :: SDL.Renderer -> IO (Animation)
loadBjFace r = loadAnimation r "bj.png" (30, 30)

loadNumbers :: SDL.Renderer -> IO (SpriteSheet)
loadNumbers r = loadSpriteSheet r "hud-numbers.png" (8, 16)

loadHudWeapons :: SDL.Renderer -> IO (SpriteSheet)
loadHudWeapons r = loadSpriteSheet r "hud-weapons.png" (48, 24)

loadWallDatas :: SDL.Renderer -> IO (WallMaterialData)
loadWallDatas r = do
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
    rectToSize :: (WallMaterial, (SDL.Texture, SDL.Rectangle CInt)) -> (WallMaterial, (SDL.Texture, (CInt, CInt)))
    rectToSize (m, (t, SDL.Rectangle _ (SDL.V2 width height))) = (m, (t, (width, height)))

loadEnvItemsData :: SDL.Renderer -> IO (Map EnvItemType (SDL.Texture, SDL.Rectangle CInt))
loadEnvItemsData r = do
  drum <- loadTexture r "drum.png"
  flag <- loadTexture r "flag.png"
  light <- loadTexture r "light.png"
  return (fromList [ (Drum, drum), (Flag, flag), (Light, light)])

loadWeaponData :: SDL.Renderer -> IO (Map String Animation)
loadWeaponData r = do
  pistol <- loadAnimation r "pistol.png" (151, 151)
  return (fromList [("Pistol", pistol)])

loadTexture :: SDL.Renderer -> FilePath -> IO (SDL.Texture, SDL.Rectangle CInt)
loadTexture r p = do
  t <- SDL.Image.loadTexture r ("assets/" ++ p)
  i <- SDL.queryTexture t
  let w = (SDL.Video.Renderer.textureWidth i)
  let h = (SDL.Video.Renderer.textureHeight i)
  return (t, mkSDLRect 0 0 w h)

loadSpriteSheet :: SDL.Renderer -> FilePath -> SpriteSize -> IO SpriteSheet
loadSpriteSheet r p sSize = do
  (t, SDL.Rectangle _ tSize) <- loadTexture r p
  return (fromJust (createSpriteSheet t tSize sSize))

loadAnimation :: SDL.Renderer -> FilePath -> SpriteSize -> IO Animation
loadAnimation r p sSize = do
  (t, SDL.Rectangle _ tSize) <- loadTexture r p
  return (createAnimation (fromJust (createSpriteSheet t tSize sSize)))

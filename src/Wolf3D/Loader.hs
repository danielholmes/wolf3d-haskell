module Wolf3D.Loader (loadRenderData) where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Video.Renderer
import Wolf3D.Display.Data
import Wolf3D.WorldData
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
  hBase <- loadHudBaseData r
  face <- loadBjFace r
  nums <- loadNumbers r
  hWeapons <- loadHudWeapons r
  return (RenderData w i weapons hBase face nums hWeapons)

loadHudBaseData :: SDL.Renderer -> IO (SDL.Texture, SDL.Rectangle CInt)
loadHudBaseData r = loadTexture r "hud-base.png"

loadBjFace :: SDL.Renderer -> IO (Animation)
loadBjFace r = loadAnimation r "bj.png" (30, 30)

loadNumbers :: SDL.Renderer -> IO (SpriteSheet)
loadNumbers r = loadSpriteSheet r "hud-numbers.png" (8, 16)

loadHudWeapons :: SDL.Renderer -> IO (SpriteSheet)
loadHudWeapons r = loadSpriteSheet r "hud-weapons.png" (48, 24)

loadWallDatas :: SDL.Renderer -> IO (WallData)
loadWallDatas r = do
  blue <- loadWallSheet r "blue1.png"
  blue2 <- loadWallSheet r "blue2.png"
  grey <- loadWallSheet r "grey1.png"
  grey2 <- loadWallSheet r "grey2.png"
  let entries = [ (Blue1, blue)
                , (Blue2, blue2)
                , (Grey1, grey)
                , (Grey2, grey2)]
  return (fromList entries)

loadEnvItemsData :: SDL.Renderer -> IO (Map EnvItemType (SDL.Texture, SDL.Rectangle CInt))
loadEnvItemsData r = do
  drum <- loadTexture r "drum.png"
  flag <- loadTexture r "flag.png"
  light <- loadTexture r "light.png"
  return (fromList [ (Drum, drum), (Flag, flag), (Light, light)])

loadWeaponData :: SDL.Renderer -> IO (Map String Animation)
loadWeaponData r = do
  pistol <- loadAnimation r "pistol.png" (128, 60)
  return (fromList [("Pistol", pistol)])

loadTexture :: SDL.Renderer -> FilePath -> IO (SDL.Texture, SDL.Rectangle CInt)
loadTexture r p = do
  t <- SDL.Image.loadTexture r ("assets/" ++ p)
  i <- SDL.queryTexture t
  let w = (SDL.Video.Renderer.textureWidth i)
  let h = (SDL.Video.Renderer.textureHeight i)
  return (t, mkSDLRect 0 0 w h)

loadWallSheet :: SDL.Renderer -> FilePath -> IO SpriteSheet
loadWallSheet r p = do
  (t, SDL.Rectangle _ tSize@(SDL.V2 tW _)) <- loadTexture r p
  let sSize = (tW, tW)
  return (fromJust (createSpriteSheet t tSize sSize))

loadSpriteSheet :: SDL.Renderer -> FilePath -> SpriteSize -> IO SpriteSheet
loadSpriteSheet r p sSize = do
  (t, SDL.Rectangle _ tSize) <- loadTexture r p
  return (fromJust (createSpriteSheet t tSize sSize))

loadAnimation :: SDL.Renderer -> FilePath -> SpriteSize -> IO Animation
loadAnimation r p sSize = do
  (t, SDL.Rectangle _ tSize) <- loadTexture r p
  return (createAnimation (fromJust (createSpriteSheet t tSize sSize)))

module Wolf3D.Loader (loadRenderData) where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Video.Renderer
import Wolf3D.Engine
import Wolf3D.Geom
import Wolf3D.Display
import Wolf3D.Sim
import Wolf3D.Animation
import Wolf3D.SDLUtils
import Data.Maybe
import Data.Map (Map, fromList)
import Foreign.C.Types (CInt)


tan30 :: Double
tan30 = tan (pi / 6)

hudBorderTop :: (Int, Int)
hudBorderTop = (8, 4)

hudBarHeight :: Int
hudBarHeight = 40

loadRenderData :: SDL.Renderer -> (Int, Int) -> IO RenderData
loadRenderData r s@(windowWidth, windowHeight) = do
  w <- loadWallDatas r
  i <- loadEnvItemsData r
  weapons <- loadWeaponData r
  hudPlaceholder <- loadHudData r
  let actionWidth = windowWidth - 2 * (fst hudBorderTop)
  let actionHeight = windowHeight - 2 * (snd hudBorderTop) - hudBarHeight
  let actionArea = IntRectangle hudBorderTop (actionWidth, actionHeight)
  let halfSize = (actionWidth `div` 2, actionHeight `div` 2)
  let distToProjPlane = fromIntegral (actionWidth `div` 2) / tan30
  return (RenderData s actionArea halfSize distToProjPlane w i weapons hudPlaceholder)

loadHudData :: SDL.Renderer -> IO (SDL.Texture, SDL.Rectangle CInt)
loadHudData r = loadTexture r "hud-placeholder.png"

loadWallDatas :: SDL.Renderer -> IO (Map WallMaterial (SDL.Texture, (Int, Int)))
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
    rectToSize :: (WallMaterial, (SDL.Texture, SDL.Rectangle CInt)) -> (WallMaterial, (SDL.Texture, (Int, Int)))
    rectToSize (m, (t, SDL.Rectangle _ (SDL.V2 width height))) = (m, (t, (fromIntegral width, fromIntegral height)))

loadEnvItemsData :: SDL.Renderer -> IO (Map EnvItemType (SDL.Texture, SDL.Rectangle CInt))
loadEnvItemsData r = do
  drum <- loadTexture r "drum.png"
  flag <- loadTexture r "flag.png"
  light <- loadTexture r "light.png"
  return (fromList [ (Drum, drum), (Flag, flag), (Light, light)])

loadWeaponData :: SDL.Renderer -> IO (Map String Animation)
loadWeaponData r = do
  pistol <- loadAnimation r "pistol.png" (256, 256)
  return (fromList [("Pistol", pistol)])

loadTexture :: SDL.Renderer -> FilePath -> IO (SDL.Texture, SDL.Rectangle CInt)
loadTexture r p = do
  t <- SDL.Image.loadTexture r ("assets/" ++ p)
  i <- SDL.queryTexture t
  let w = fromIntegral (SDL.Video.Renderer.textureWidth i)
  let h = fromIntegral (SDL.Video.Renderer.textureHeight i)
  return (t, mkSDLRect 0 0 w h)

loadAnimation :: SDL.Renderer -> FilePath -> SpriteSize -> IO Animation
loadAnimation r p sSize = do
  (t, SDL.Rectangle _ tSize) <- loadTexture r p
  return (createAnimation (fromJust (createSpriteSheet t tSize sSize)))

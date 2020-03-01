module Wolf3D.Display.Hud (renderHud) where

import Wolf3D.Display.Data
import Wolf3D.WorldData
import Wolf3D.SDLUtils
import Wolf3D.Animation
import Foreign.C.Types (CInt)
import qualified SDL
import Control.Monad (void)

bjFaceX :: CInt
bjFaceX = 136

bjFaceY :: CInt
bjFaceY = fromIntegral (screenHeight - 35)

hudWeaponX :: CInt
hudWeaponX = 254

hudWeaponY :: CInt
hudWeaponY = fromIntegral (screenHeight - 33)

renderHud :: SDL.Renderer -> RenderData -> Hero -> IO ()
renderHud r d h = do
  let textY = 176
  renderHudBase r d
  renderHudFace r d h
  renderHudNum r d (34, textY) 1 -- Floor
  renderHudNum r d (120, textY) 3 -- Lives
  renderHudNum r d (96, textY) 0 -- Score
  renderHudNum r d (192, textY) 100 -- Health
  renderHudNum r d (232, textY) 8 -- Ammo
  renderHudWeapon r d

renderHudBase :: SDL.Renderer -> RenderData -> IO ()
renderHudBase r (RenderData {hudBase=(baseTexture, sourceRect)}) = do
  SDL.copy r baseTexture (Just sourceRect) (Just destRect)
  where
    destPos = SDL.P (SDL.V2 0 0)
    destSize = SDL.V2 screenWidth screenHeight
    destRect = SDL.Rectangle destPos destSize

renderHudNum :: SDL.Renderer -> RenderData -> (CInt, CInt) -> Int -> IO ()
renderHudNum r d (x, y) num = do
  SDL.copy r (spriteSheetTexture numsSheet) (Just from) (Just destRect)
  case length start of 0 -> return ()
                       _ -> void (renderHudNum r d (x - width, y) ((read start) :: Int))
  where
    numStr = show num
    (start, lastChar) = splitAt ((length numStr) - 1) numStr
    lastNum = (read lastChar) :: Int
    numsSheet = numbers d
    from@(SDL.Rectangle _ (SDL.V2 width height)) = getSpriteSheetLocation numsSheet lastNum
    destRect = mkSDLRect (x - width) y width height

-- TODO: Draw dead face if no health left
-- TODO: Alter which face based on health
--		StatusDrawPic (17,4,FACE1APIC+3*((100-gamestate.health)/16)+gamestate.faceframe);
renderHudFace :: SDL.Renderer -> RenderData -> Hero -> IO ()
renderHudFace r (RenderData {bjFace=a}) (Hero{heroFace=HeroFace _ faceFrame}) = do
  SDL.copy r texture (Just sourceRect) (Just destRect)
  where
    texture = animationTexture a
    sourceRect@(SDL.Rectangle _ (SDL.V2 tW tH)) = getAnimationFrame a faceFrame
    destRect = mkSDLRect bjFaceX bjFaceY tW tH


renderHudWeapon :: SDL.Renderer -> RenderData -> IO ()
renderHudWeapon r (RenderData {hudWeapons=w}) = do
  SDL.copy r texture (Just sourceRect) (Just destRect)
  where
    texture = spriteSheetTexture w
    -- TODO: Use current weapon
    sourceRect@(SDL.Rectangle _ (SDL.V2 tW tH)) = getSpriteSheetLocation w 1
    destRect = mkSDLRect hudWeaponX hudWeaponY tW tH

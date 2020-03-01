module Wolf3D.Animation (
  SpriteSheet,
  createSpriteSheet,
  spriteSheetTexture,
  Animation,
  SpriteSize,
  animationTexture,
  createAnimation,
  getSpriteSheetLocation,
  getAnimationFrame,
  SpriteSheetFrame
) where

import qualified SDL
import Foreign.C.Types (CInt)


type TextureSize = (CInt, CInt)
type SpriteSize = (CInt, CInt)
type SpriteLocation = SDL.Rectangle CInt
type SpriteIndex = Int
data SpriteSheet = SpriteSheet SDL.Texture TextureSize [SpriteLocation]
type SpriteSheetFrame = (SDL.Texture, SpriteLocation)

type AnimationFrame = SpriteLocation
data Animation = Animation SpriteSheet

createSpriteSheet :: SDL.Texture -> SDL.V2 CInt -> SpriteSize -> Maybe SpriteSheet
createSpriteSheet t cSize@(SDL.V2 cW cH) sSize@(sW, sH)
  | cW `mod` sW == 0 && cH `mod` sH == 0 = Just (SpriteSheet t (cW, cH) locations)
  | otherwise                            = Nothing
  where
    locations = createSpriteLocations cSize sSize

createSpriteLocations :: SDL.V2 CInt -> SpriteSize -> [SpriteLocation]
createSpriteLocations (SDL.V2 w h) (fW, fH) = concatMap createRowLocations rows
  where
    cols = [0..((w `div` fW) - 1)]
    rows = [0..((h `div` fH) - 1)]
    sSize = SDL.V2 fW fH

    createRowLocations :: CInt -> [SpriteLocation]
    createRowLocations row = map (createColLocation row) cols

    createColLocation :: CInt -> CInt -> SpriteLocation
    createColLocation row col = SDL.Rectangle colPos sSize
      where colPos = SDL.P (SDL.V2 (col * fW) (row * fH))

getSpriteSheetLocation :: SpriteSheet -> SpriteIndex -> SpriteLocation
getSpriteSheetLocation (SpriteSheet _ _ ls) i = ls !! i

spriteSheetTexture :: SpriteSheet -> SDL.Texture
spriteSheetTexture (SpriteSheet t _ _) = t

createAnimation :: SpriteSheet -> Animation
createAnimation = Animation

animationTexture :: Animation -> SDL.Texture
animationTexture (Animation s) = spriteSheetTexture s

getAnimationFrame :: Animation -> Int -> AnimationFrame
getAnimationFrame (Animation s) i = getSpriteSheetLocation s i

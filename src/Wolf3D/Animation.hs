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


type TextureSize = (Int, Int)
type SpriteSize = (Int, Int)
type SpriteLocation = SDL.Rectangle CInt
type SpriteIndex = Int
data SpriteSheet = SpriteSheet SDL.Texture TextureSize [SpriteLocation]
type SpriteSheetFrame = (SDL.Texture, SpriteLocation)

type AnimationFrame = SpriteLocation
data Animation = Animation SpriteSheet

createSpriteSheet :: SDL.Texture -> SDL.V2 CInt -> SpriteSize -> Maybe SpriteSheet
createSpriteSheet t (SDL.V2 cW cH) sSize@(sW, sH)
  | w `mod` sW == 0 && h `mod` sH == 0 = Just (SpriteSheet t (w, h) locations)
  | otherwise                          = Nothing
  where
    w = fromIntegral cW
    h = fromIntegral cH
    locations = createSpriteLocations (w, h) sSize

createSpriteLocations :: TextureSize -> SpriteSize -> [SpriteLocation]
createSpriteLocations (w, h) (fW, fH) = concatMap createRowLocations rows
  where
    cols = [0..((w `div` fW) - 1)]
    rows = [0..((h `div` fH) - 1)]
    sSize = SDL.V2 (fromIntegral fW) (fromIntegral fH)

    createRowLocations :: Int -> [SpriteLocation]
    createRowLocations row = map (createColLocation row) cols

    createColLocation :: Int -> Int -> SpriteLocation
    createColLocation row col = SDL.Rectangle colPos sSize
      where colPos = SDL.P (SDL.V2 (fromIntegral (col * fW)) (fromIntegral (row * fH)))

getSpriteSheetLocation :: SpriteSheet -> SpriteIndex -> SpriteLocation
getSpriteSheetLocation (SpriteSheet _ _ ls) i = ls !! i

spriteSheetTexture :: SpriteSheet -> SDL.Texture
spriteSheetTexture (SpriteSheet t _ _) = t

createAnimation :: SpriteSheet -> Animation
createAnimation = Animation

animationTexture :: Animation -> SDL.Texture
animationTexture (Animation s) = spriteSheetTexture s

getAnimationFrame :: Animation -> Double -> AnimationFrame
getAnimationFrame (Animation s@(SpriteSheet _ _ ls)) progress
  | progress >= 0 && progress <= 1 = getSpriteSheetLocation s locationIndex
  | otherwise                      = error ("Invalid progress " ++ show progress)
  where
    locationIndex = round (progress * fromIntegral (length ls - 1))

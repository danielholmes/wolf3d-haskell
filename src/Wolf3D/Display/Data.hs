module Wolf3D.Display.Data (
  RenderData (..),
  WallRayHit (..),
  WallData,
  HitDirection (..),
  CIntRectangle,
  
  fieldOfView,
  
  screenHeight,
  screenWidth,
  
  actionAreaX,
  actionAreaY,
  actionWidth,
  actionHeight,
  halfActionWidth,
  halfActionHeight,
  actionArea,
  intRectPos,
  intRectX,
  intRectY
) where

import qualified Data.Map as M
import Foreign.C.Types (CInt)
import qualified SDL
import Wolf3D.Animation
import Wolf3D.WorldData

type WallData = M.Map Wall SpriteSheet
type EnvItemData = M.Map EnvItemType (SDL.Texture, SDL.Rectangle CInt)
type WeaponData = M.Map String Animation
data RenderData = RenderData {wallTextures :: WallData
                             , itemTextures :: EnvItemData
                             , weaponTextures :: WeaponData
                             , hudBase :: (SDL.Texture, SDL.Rectangle CInt)
                             , bjFace :: Animation
                             , numbers :: SpriteSheet
                             , hudWeapons :: SpriteSheet }

data CIntRectangle = CIntRectangle (CInt, CInt) (CInt, CInt)

data HitDirection = Horizontal | Vertical
  deriving (Show, Eq, Ord)

data WallRayHit = WallRayHit {material :: Wall
                              , direction :: HitDirection
                              , distance :: Int
                              , tilePosition :: Int}
  deriving (Eq, Show)

fieldOfView :: Angle
fieldOfView = 75

intRectX :: CIntRectangle -> CInt
intRectX (CIntRectangle (x, _) _) = x

intRectY :: CIntRectangle -> CInt
intRectY (CIntRectangle (_, y) _) = y

intRectPos :: CIntRectangle -> (CInt, CInt)
intRectPos (CIntRectangle pos _) = pos

screenWidth :: CInt
screenWidth = 320 :: CInt

screenHeight :: CInt
screenHeight = 200 :: CInt

hudBorderTop :: (CInt, CInt)
hudBorderTop = (8, 4)

hudBarHeight :: CInt
hudBarHeight = 40

actionWidth :: CInt
actionWidth = screenWidth - 2 * (fst hudBorderTop)

actionHeight :: CInt
actionHeight = screenHeight - 2 * (snd hudBorderTop) - hudBarHeight

actionAreaY :: CInt
actionAreaY = snd hudBorderTop

actionAreaX :: CInt
actionAreaX = fst hudBorderTop

actionArea :: CIntRectangle
actionArea = CIntRectangle hudBorderTop (actionWidth, actionHeight)

halfActionHeight :: CInt
halfActionHeight = fromIntegral (actionHeight `div` 2)

halfActionWidth :: CInt
halfActionWidth = fromIntegral (actionWidth `div` 2)

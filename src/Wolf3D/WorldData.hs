module Wolf3D.WorldData (
  World (..),
  WorldTicks,
  WallMap,
  Wall (..),
  TileCoord,
  Ceiling (..),
  EnvItemType (..),
  EnvItem (..),
  
  HeroActionsState (..),
  Hero (..),
  SnappedRotation,
  HeroAction (..),
  Weapon (..),
  
  worldWallMap,
  worldCeilingColor,
  worldTicks,
  worldHero,
  worldHeroWeapon,
  worldEnvItems,
  
  Angle,
  FineAngle,
  minDist,
  angles,
  fineAngles,
  bindAngle,  
  fineToNormalAngle,
  normalToFineAngle,
  
  tileGlobalSize,
  tileToGlobalShift,
  tileCoordToGlobalPos,
  tileCentreGlobalOffset,
  tileCoordToCentreGlobalPos
) where

import Data.Vector
import Data.Array
import Data.Bits

type UsingWeapon = Bool
data Weapon = Pistol (Maybe WorldTicks) UsingWeapon
  deriving (Eq, Show)

data HeroActionsState = HeroActionsState
  { heroActionsStateMoveForward  :: Bool
  , heroActionsStateMoveBackward :: Bool
  , heroActionsStateTurnLeft     :: Bool
  , heroActionsStateTurnRight    :: Bool
  , heroActionsStateUseWeapon    :: Bool
  }
  deriving (Show, Eq)

data HeroAction = MoveForward | MoveBackward | TurnLeft | TurnRight | UseWeapon

type Position = Vector2
type SnappedRotation = Int
type RotationRemainder = Int
data Hero = Hero {position :: Position
                 , snappedRotation :: SnappedRotation
                 , rotationRemainder :: RotationRemainder
                 , actionsState :: HeroActionsState
                 , weapon :: Weapon}
  deriving (Show, Eq)

data EnvItemType = Drum | Flag | Light
 deriving (Show, Eq, Ord)

data EnvItem = EnvItem EnvItemType Vector2
 deriving (Show, Eq)

type FineAngle = Int
type Angle = Int

data Wall = Grey1 | Grey2 | Blue1 | Blue2
  deriving (Show, Eq, Ord)

type TileCoord = (Int, Int)

data Ceiling = GreyCeiling | PurpleCeiling | GreenCeiling | YellowCeiling
  deriving (Show, Eq, Ord)

type WorldTicks = Int
type WallMap = Array (Int, Int) (Maybe Wall)
-- Tried record syntax for this and failed
data World = World Ceiling WallMap Hero [EnvItem] WorldTicks

worldWallMap :: World -> WallMap
worldWallMap (World _ wm _ _ _) = wm

worldCeilingColor :: World -> Ceiling
worldCeilingColor (World c _ _ _ _) = c

worldTicks :: World -> Int
worldTicks (World _ _ _ _ t) = t

worldHero :: World -> Hero
worldHero (World _ _ h _ _) = h

worldHeroWeapon :: World -> Weapon
worldHeroWeapon = weapon . worldHero

worldEnvItems :: World -> [EnvItem]
worldEnvItems (World _ _ _ is _) = is

tileGlobalSize :: Int
tileGlobalSize = 1 `shiftL` 16

tileToGlobalShift :: Int
tileToGlobalShift =  16

tileCentreGlobalOffset :: Vector2
tileCentreGlobalOffset = Vector2 (fromIntegral (tileGlobalSize `div` 2)) (fromIntegral (tileGlobalSize `div` 2))

tileCoordToCentreGlobalPos :: TileCoord -> Vector2
tileCoordToCentreGlobalPos p = (tileCoordToGlobalPos p) + tileCentreGlobalOffset

tileCoordToGlobalPos :: TileCoord -> Vector2
tileCoordToGlobalPos (tileX, tileY) = Vector2 (fromIntegral worldX) (fromIntegral worldY)
  where
    worldX = tileX `shiftL` tileToGlobalShift
    worldY = tileY `shiftL` tileToGlobalShift

angles :: Angle
angles = 360

fineAngles :: FineAngle
fineAngles = 3600

normalToFineAngle :: Int -> Int
normalToFineAngle a = fineAngles `div` angles * a

fineToNormalAngle :: Int -> Double
fineToNormalAngle f = fromIntegral f * fromIntegral angles / fromIntegral fineAngles

bindAngle :: Angle -> Angle
bindAngle a
  | a > angles = a - angles
  | a < 0      = a + angles
  | otherwise  = a

minDist :: Int
minDist = 0x5800

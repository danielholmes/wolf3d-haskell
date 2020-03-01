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
  HeroFace (..),
  HeroState (..),
  SnappedRotation,
  HeroAction (..),
  Weapon (..),
  
  isHittingWall,

  Angle,
  FineAngle,
  worldHeroWeapon,
  minDist,
  angles,
  fineAngles,
  bindAngle,
  fineToNormalAngle,
  normalToFineAngle,
  worldTickRandomNum,
  worldTickNextRandomNum,

  tileGlobalSize,
  tileToGlobalShift,
  tileCoordToGlobalPos,
  tileCentreGlobalOffset,
  tileCoordToCentreGlobalPos
) where

import Data.Vector
import Data.Array
import Data.Bits
import Data.Maybe


type UsingWeapon = Bool
data Weapon = Pistol (Maybe WorldTicks) UsingWeapon
  deriving (Eq, Show)

data HeroActionsState = HeroActionsState
  { heroActionsStateMoveForward  :: Bool
  , heroActionsStateMoveBackward :: Bool
  , heroActionsStateTurnLeft     :: Bool
  , heroActionsStateTurnRight    :: Bool
  , heroActionsStateStrafe       :: Bool
  , heroActionsStateUseWeapon    :: Bool }
  deriving (Show, Eq)

data HeroAction = MoveForward | MoveBackward | TurnLeft | TurnRight | UseWeapon | Strafe

data HeroState = HeroDefault | HeroAttack
  deriving (Show, Eq)
type Position = Vector2
type SnappedRotation = Int
type RotationRemainder = Int

type FaceCount = Int
type FaceFrame = Int
data HeroFace = HeroFace FaceCount FaceFrame deriving (Show, Eq)

data Hero = Hero
  { heroState :: HeroState
  , heroFace :: HeroFace
  , position :: Position
  , snappedRotation :: SnappedRotation
  , rotationRemainder :: RotationRemainder
  , actionsState :: HeroActionsState
  , weapon :: Weapon }
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
data World = World
  { worldCeiling :: Ceiling
  , worldWallMap :: WallMap
  , worldHero :: Hero
  , worldEnvItems :: [EnvItem]
  , worldTicks :: WorldTicks}

worldHeroWeapon :: World -> Weapon
worldHeroWeapon w = weapon (worldHero w)

-- Would be better in world, but results in cyclic dependency
type ActorSize = Int

isHittingWall :: World -> Vector2 -> ActorSize -> Bool
isHittingWall (World {worldWallMap=wm}) (Vector2 x y) s = any isJust (map (wm!) tiles)
  where
    xTileLow = ((round x) - s) `shiftR` tileToGlobalShift
    yTileLow = ((round y) - s) `shiftR` tileToGlobalShift
    xTileHigh = ((round x) + s) `shiftR` tileToGlobalShift
    yTileHigh = ((round y) + s) `shiftR` tileToGlobalShift
    tiles = [(xTileLow, yTileLow), (xTileLow, yTileHigh), (xTileHigh, yTileLow), (xTileHigh, yTileHigh)]

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

-- Would be better in world, by cyclic deps
worldTickRandomNum :: World -> Int
worldTickRandomNum (World{worldTicks=t}) = randomTable!!(t `mod` length randomTable)

worldTickNextRandomNum :: World -> Int
worldTickNextRandomNum (World{worldTicks=t}) = randomTable!!((t + 1) `mod` length randomTable)

-- Carmack's original random table
randomTable :: [Int]
randomTable = [  0,   8, 109, 220, 222, 241, 149, 107,  75, 248, 254, 140,  16,  66
             ,  74,  21, 211,  47,  80, 242, 154,  27, 205, 128, 161,  89,  77,  36
             ,  95, 110,  85,  48, 212, 140, 211, 249,  22,  79, 200,  50,  28, 188
             ,  52, 140, 202, 120,  68, 145,  62,  70, 184, 190,  91, 197, 152, 224
             , 149, 104,  25, 178, 252, 182, 202, 182, 141, 197,   4,  81, 181, 242
             , 145,  42,  39, 227, 156, 198, 225, 193, 219,  93, 122, 175, 249,   0
             , 175, 143,  70, 239,  46, 246, 163,  53, 163, 109, 168, 135,   2, 235
             ,  25,  92,  20, 145, 138,  77,  69, 166,  78, 176, 173, 212, 166, 113
             ,  94, 161,  41,  50, 239,  49, 111, 164,  70,  60,   2,  37, 171,  75
             , 136, 156,  11,  56,  42, 146, 138, 229,  73, 146,  77,  61,  98, 196
             , 135, 106,  63, 197, 195,  86,  96, 203, 113, 101, 170, 247, 181, 113
             ,  80, 250, 108,   7, 255, 237, 129, 226,  79, 107, 112, 166, 103, 241
             ,  24, 223, 239, 120, 198,  58,  60,  82, 128,   3, 184,  66, 143, 224
             , 145, 224,  81, 206, 163,  45,  63,  90, 168, 114,  59,  33, 159,  95
             ,  28, 139, 123,  98, 125, 196,  15,  70, 194, 253,  54,  14, 109, 226
             ,  71,  17, 161,  93, 186,  87, 244, 138,  20,  52, 123, 251,  26,  36
             ,  17,  46,  52, 231, 232,  76,  31, 221,  84,  37, 216, 165, 212, 106
             , 197, 242,  98,  43,  39, 175, 254, 145, 190,  84, 118, 222, 187, 136
             , 120, 163, 236, 249]

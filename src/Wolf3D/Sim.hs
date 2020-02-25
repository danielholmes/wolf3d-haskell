{-# LANGUAGE GADTs #-}
module Wolf3D.Sim (
  -- WorldData
  SimEntity (simUpdate),
  World,
  WorldTicks,
  WallMap,
  Wall (..),
  TileCoord,
  Ceiling (..),
  createWorld,
  worldEntities,
  updateWorldEntities,
  ticWorldTicks,
  emptyWallMap,
  worldTics,
  worldCeilingColor,
  worldWallMap,
  tickWorld,
  tickWorldNTimes,

  tileCoordToGlobalPos,
  tileCoordToCentreGlobalPos,
  tileGlobalSize,
  tileToGlobalShift,
    
  HeroAction (..),
  Hero (position, snappedRotation, actionsState, weapon),
  SnappedRotation,
  EnvItemType (Drum, Light, Flag),
  EnvItem (EnvItem),

  Wolf3DSimEntity (SEEnvItem, SEHero),
  worldHero,
  worldHeroWeapon,
  worldEnvItems,
  worldEnvItemsTouching,
  updateWorldHeroActionsState,

  Weapon (Pistol),
  lastTimeWeaponUsed,

  angles,
  fineAngles,
  Angle,
  FineAngle,
  normalToFineAngle,
  fineToNormalAngle,
  createHero,
  createHeroFromTilePosition,
  moveHero,
  rotateHero,
  HeroActionsState,
  staticHeroActionsState,
  modifyHeroActionState,
  updateHeroActionsState,

  itemRectangle
) where

import Wolf3D.Geom
import Data.Vector
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Bits
import Data.Array


class SimEntity i where
  simUpdate :: World a -> i -> i

-- TODO: See if way of moving wall materials outside of engine
data Wall = Grey1 | Grey2 | Blue1 | Blue2
  deriving (Show, Eq, Ord)

type TileCoord = (Int, Int)

data Ceiling = GreyCeiling | PurpleCeiling | GreenCeiling | YellowCeiling
  deriving (Show, Eq, Ord)

type WorldTicks = Int
type WallMap = Array (Int, Int) (Maybe Wall)
-- Tried record syntax for this and failed
data World i where
  World :: (SimEntity i) => Ceiling -> WallMap -> [i] -> WorldTicks -> World i

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

emptyWallMap :: Int -> Int -> WallMap
emptyWallMap w h = listArray ((0, 0), (w - 1, h - 1)) (replicate (w * h) Nothing)

createWorld :: (SimEntity i) => Ceiling -> WallMap -> i -> [i] -> World i
createWorld c wm h is = World c wm (h:is) 0 

tickWorld :: World i -> World i
tickWorld world@(World _ _ is _) = ticWorldTicks updatedWorld
  where
    updatedItems = map (simUpdate world) is
    updatedWorld = updateWorldEntities world updatedItems

tickWorldNTimes :: World i -> Int -> Maybe (World i)
tickWorldNTimes w n
  | n == 0    = Nothing
  | otherwise = Just (foldr foldStep w [1..n])
    where
      foldStep :: Int -> World i -> World i
      foldStep _ = tickWorld

updateWorldEntities :: World i -> [i] -> World i
updateWorldEntities (World c wm _ t) i = World c wm i t

worldWallMap :: World i -> WallMap
worldWallMap (World _ wm _ _) = wm

worldEntities :: (SimEntity i) => World i -> [i]
worldEntities (World _ _ is _) = is

worldCeilingColor :: World i -> Ceiling
worldCeilingColor (World c _ _ _) = c

worldTics :: World i -> Int
worldTics (World _ _ _ t) = t

ticWorldTicks :: World i -> World i
ticWorldTicks (World c wm is ticks) = World c wm is (ticks + 1)

{-----------------------------------------------------------------------------------------------------------------------
WorldData
-----}
data Wolf3DSimEntity = SEEnvItem EnvItem | SEHero Hero
  deriving (Show, Eq)

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

angles :: Angle
angles = 360

fineAngles :: FineAngle
fineAngles = 3600

normalToFineAngle :: Int -> Int
normalToFineAngle a = fineAngles `div` angles * a

fineToNormalAngle :: Int -> Double
fineToNormalAngle f = fromIntegral f * fromIntegral angles / fromIntegral fineAngles

angleScale :: Int
angleScale = 20

moveScale :: Int
moveScale = 150

backMoveScale :: Int
backMoveScale = 100

heroBaseMovePerFrame :: Int
heroBaseMovePerFrame = 35

minDist :: Int
minDist = 0x5800

{-----------------------------------------------------------------------------------------------------------------------
 General
-----------------------------------------------------------------------------------------------------------------------}
instance SimEntity Wolf3DSimEntity where
  simUpdate w (SEEnvItem i) = SEEnvItem (simUpdate w i)
  simUpdate w (SEHero i) = SEHero (simUpdate w i)

worldHero :: World Wolf3DSimEntity -> Hero
worldHero w = fromJust (fmap (\(SEHero h) -> h) (find (\i -> case i of (SEHero _) -> True; _ -> False) (worldEntities w)))

worldHeroWeapon :: World Wolf3DSimEntity -> Weapon
worldHeroWeapon = weapon . worldHero

worldEnvItems :: World Wolf3DSimEntity -> [EnvItem]
worldEnvItems w = map (\(SEEnvItem e) -> e) (filter (\i -> case i of (SEEnvItem _) -> True; _ -> False) (worldEntities w))

updateWorldHeroActionsState :: World Wolf3DSimEntity -> HeroActionsState -> World Wolf3DSimEntity
updateWorldHeroActionsState w a = updateWorldHero w (updateHeroActionsState a)

updateWorldHero :: World Wolf3DSimEntity -> (Hero -> Hero) -> World Wolf3DSimEntity
updateWorldHero w op = updateWorldEntities w newItems
  where
    newItems = foldr foldStep [] (worldEntities w)
    foldStep :: Wolf3DSimEntity -> [Wolf3DSimEntity] -> [Wolf3DSimEntity]
    foldStep (SEHero h) accu = SEHero (op h) : accu
    foldStep i accu = i : accu

worldEnvItemsTouching :: Rectangle -> World Wolf3DSimEntity -> [EnvItem]
worldEnvItemsTouching r w = filter (itemIsTouching r) (worldEnvItems w)

itemIsTouching :: Rectangle -> EnvItem -> Bool
itemIsTouching r i = rectangleOverlapsRectangle r (itemRectangle i)

{-----------------------------------------------------------------------------------------------------------------------
 Weapon
-----------------------------------------------------------------------------------------------------------------------}
instance SimEntity Weapon where
  simUpdate w wn
    | isUsingWeapon wn && canUseWeapon (worldTics w) wn = useWeapon w wn
    | otherwise                                         = wn

canUseWeapon :: WorldTicks -> Weapon -> Bool
canUseWeapon t w = all (< t - timeBetweenUses w) (lastTimeWeaponUsed w)

useWeapon :: World a -> Weapon -> Weapon
useWeapon w (Pistol _ s) = Pistol (Just (worldTics w + 1)) s

lastTimeWeaponUsed :: Weapon -> Maybe WorldTicks
lastTimeWeaponUsed (Pistol t _) = t

timeBetweenUses :: Weapon -> Int
timeBetweenUses (Pistol _ _) = 600

isUsingWeapon :: Weapon -> Bool
isUsingWeapon (Pistol _ s) = s

beingUsed :: Weapon -> Weapon
beingUsed (Pistol t _) = Pistol t True

notBeingUsed :: Weapon -> Weapon
notBeingUsed (Pistol t _) = Pistol t False

{-----------------------------------------------------------------------------------------------------------------------
 Hero
-----------------------------------------------------------------------------------------------------------------------}
staticHeroActionsState :: HeroActionsState
staticHeroActionsState = HeroActionsState False False False False False

modifyHeroActionState :: HeroActionsState -> HeroAction -> Bool -> HeroActionsState
modifyHeroActionState (HeroActionsState _ d l r s) MoveForward a = HeroActionsState a d l r s
modifyHeroActionState (HeroActionsState u _ l r s) MoveBackward a = HeroActionsState u a l r s
modifyHeroActionState (HeroActionsState u d _ r s) TurnLeft a = HeroActionsState u d a r s
modifyHeroActionState (HeroActionsState u d l _ s) TurnRight a = HeroActionsState u d l a s
modifyHeroActionState (HeroActionsState u d l r _) UseWeapon a = HeroActionsState u d l r a

instance SimEntity Hero where
  simUpdate w h@(Hero {actionsState=has}) = h3
    where
      rotationDelta = heroRotationDelta has
      h1 = rotateHero h rotationDelta

      movementDelta = heroMoveDelta has
      movementScale = if movementDelta < 0 then moveScale else backMoveScale
      -- TODO: set a thrustspeed for AI to use later
      -- thrustspeed += speed;
      velocity = movementScale * movementDelta
      -- TODO
      -- ClipMove(player,xmove,ymove);
      -- player->tilex = player->x >> TILESHIFT;    // scale to tile values
      -- player->tiley = player->y >> TILESHIFT;
      -- offset = farmapylookup[player->tiley]+player->tilex;
      -- player->areanumber = *(mapsegs[0] + offset) -AREATILE;
      h2 = moveHero h1 velocity
      h3 = updateWeapon w h2

updateWeapon :: World a -> Hero -> Hero
updateWeapon w h@(Hero {actionsState=has, weapon=wn}) = h { weapon = simUpdate w (updateWeaponUsed has wn) }

updateWeaponUsed :: HeroActionsState -> Weapon -> Weapon
updateWeaponUsed has w
  | heroActionsStateUseWeapon has && not current   = beingUsed w
  | not (heroActionsStateUseWeapon has) && current = notBeingUsed w
  | otherwise                                      = w
  where current = isUsingWeapon w

createHero :: Vector2 -> Hero
createHero pos = Hero pos 0 0 staticHeroActionsState (Pistol Nothing False)

createHeroFromTilePosition :: TileCoord -> Hero
createHeroFromTilePosition p = createHero (tileCoordToCentreGlobalPos p)

moveHero :: Hero -> Int -> Hero
moveHero h 0 = h
moveHero h@(Hero {position=p, snappedRotation=sr}) velocity = h {position=newPos}
  where
    speed = abs velocity
    moveAngle = if velocity < 0 then sr else bindAngle (sr + (angles `div` 2))
    boundSpeed = if speed >= minDist * 2 then minDist * 2 - 1 else speed
    rotRad = (fromIntegral moveAngle) * degToRad
    dSpeed = fromIntegral boundSpeed
    newPos = p + Vector2 (dSpeed * (cos rotRad)) (-(dSpeed * (sin rotRad)))

updateHeroActionsState :: HeroActionsState -> Hero -> Hero
updateHeroActionsState a (Hero p sr rr _ w) = Hero p sr rr a w

heroMoveDelta :: HeroActionsState -> Int
heroMoveDelta s = forwardMovement + backwardMovement
  where
    forwardMovement = if heroActionsStateMoveForward s then (-heroBaseMovePerFrame) else 0
    backwardMovement = if heroActionsStateMoveBackward s then heroBaseMovePerFrame else 0

heroRotationDelta :: HeroActionsState -> Int
heroRotationDelta has = leftRotation + rightRotation
  where
    leftRotation = if heroActionsStateTurnLeft has then (-heroBaseMovePerFrame) else 0
    rightRotation = if heroActionsStateTurnRight has then heroBaseMovePerFrame else 0

-- Bound to 0-360
rotateHero :: Hero -> Int -> Hero
rotateHero h@(Hero {snappedRotation=sr, rotationRemainder=rr}) d = h {snappedRotation=(bindAngle newAngle), rotationRemainder=newRealRot}
  where
    newRotDelta = rr + d
    angleUnits = newRotDelta `div` angleScale
    newRealRot = newRotDelta - (angleUnits * angleScale)
    newAngle = sr - angleUnits

bindAngle :: Int -> Int
bindAngle a
  | a > angles = a - angles
  | a < 0      = a + angles
  | otherwise  = a

{-----------------------------------------------------------------------------------------------------------------------
 Environment
-----------------------------------------------------------------------------------------------------------------------}
instance SimEntity EnvItem where
 simUpdate _ = id

itemRectangle :: EnvItem -> Rectangle
itemRectangle i@(EnvItem _ o) = Rectangle (o - halfItemSize i) (itemSize i)
  where
    -- 0.005 a hack until change rendering
    itemSize :: EnvItem -> Vector2
    itemSize _ = Vector2 ((fromIntegral tileGlobalSize) * 0.05) ((fromIntegral tileGlobalSize) * 0.05)
    
    halfItemSize :: EnvItem -> Vector2
    halfItemSize iS = itemSize iS *| 0.5

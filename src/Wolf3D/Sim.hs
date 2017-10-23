module Wolf3D.Sim (
Wolf3DSimEntity (SEEnvItem, SEHero),
  worldHero,
  worldHeroWeapon,
  worldEnvItems,
  worldEnvItemsTouching,
  updateWorldHeroActionsState,

  Weapon (Pistol),
  lastTimeWeaponUsed,

  Hero,
  createHero,
  createOriginHero,
  heroPosition,
  heroRotation,
  heroWeapon,
  heroLookRay,
  moveHero,
  rotateHero,
  heroHeight,
  HeroActionsState,
  staticHeroActionsState,
  modifyHeroActionState,
  HeroAction (MoveForward, MoveBackward, TurnLeft, TurnRight, UseWeapon),
  heroActionsStateMoveForward,
  heroActionsStateMoveBackward,
  heroActionsStateTurnLeft,
  heroActionsStateTurnRight,
  updateHeroActionsState,
  heroActionsState,
  heroFieldOfViewSize,
  heroLookRayAtFieldOfViewRatio,

  EnvItemType (Drum, Flag, Light),
  EnvItem (EnvItem),
  itemRectangle,
  itemHeight,
  itemSize
) where

import SimEngine.Geom
import SimEngine.Engine
import Data.Vector
import Data.Maybe (fromJust)
import Data.List (find)


{-----------------------------------------------------------------------------------------------------------------------
 General
-----------------------------------------------------------------------------------------------------------------------}
data Wolf3DSimEntity = SEEnvItem EnvItem | SEHero Hero
  deriving (Show, Eq)

instance SimEntity Wolf3DSimEntity where
  simUpdate w t (SEEnvItem i) = SEEnvItem (simUpdate w t i)
  simUpdate w t (SEHero i) = SEHero (simUpdate w t i)

worldHero :: World Wolf3DSimEntity -> Hero
worldHero w = fromJust (fmap (\(SEHero h) -> h) (find (\i -> case i of (SEHero _) -> True; _ -> False) (worldEntities w)))

worldHeroWeapon :: World Wolf3DSimEntity -> Weapon
worldHeroWeapon = heroWeapon . worldHero

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
type UsingWeapon = Bool
data Weapon = Pistol (Maybe WorldTime) UsingWeapon
  deriving (Eq, Show)

instance SimEntity Weapon where
  simUpdate w t weapon
    | isUsingWeapon weapon && canUseWeapon (worldTime w) weapon = useWeapon w t weapon
    | otherwise                                                 = weapon

canUseWeapon :: WorldTime -> Weapon -> Bool
canUseWeapon t w = all (< t - timeBetweenUses w) (lastTimeWeaponUsed w)

useWeapon :: World a -> Int -> Weapon -> Weapon
useWeapon w t (Pistol _ s) = Pistol (Just (worldTime w + t)) s

lastTimeWeaponUsed :: Weapon -> Maybe WorldTime
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
data HeroActionsState = HeroActionsState
  { heroActionsStateMoveForward  :: Bool
  , heroActionsStateMoveBackward :: Bool
  , heroActionsStateTurnLeft     :: Bool
  , heroActionsStateTurnRight    :: Bool
  , heroActionsStateUseWeapon    :: Bool
  }
  deriving (Show, Eq)

data HeroAction = MoveForward | MoveBackward | TurnLeft | TurnRight | UseWeapon

staticHeroActionsState :: HeroActionsState
staticHeroActionsState = HeroActionsState False False False False False

modifyHeroActionState :: HeroActionsState -> HeroAction -> Bool -> HeroActionsState
modifyHeroActionState (HeroActionsState _ d l r s) MoveForward a = HeroActionsState a d l r s
modifyHeroActionState (HeroActionsState u _ l r s) MoveBackward a = HeroActionsState u a l r s
modifyHeroActionState (HeroActionsState u d _ r s) TurnLeft a = HeroActionsState u d a r s
modifyHeroActionState (HeroActionsState u d l _ s) TurnRight a = HeroActionsState u d l a s
modifyHeroActionState (HeroActionsState u d l r _) UseWeapon a = HeroActionsState u d l r a

type Position = Vector2
type Rotation = Double
data Hero = Hero Position Rotation HeroActionsState Weapon
  deriving (Show, Eq)

instance SimEntity Hero where
  simUpdate w t h@(Hero _ _ has _) = updateWeapon w t (rotateHero (moveHero h movement) rotation)
    where
      rotationDirection = updateHeroRotation has
      direction = updateHeroMoveDirection has
      heroMoveMetresPerSec = 8
      movement = direction * fromIntegral (t * heroMoveMetresPerSec)
      heroRotatePerMilli = 0.002
      rotation = rotationDirection * fromIntegral t * heroRotatePerMilli

updateWeapon :: World a -> Int -> Hero -> Hero
updateWeapon w t (Hero p r has weapon) = Hero p r has (simUpdate w t (updateWeaponUsed has weapon))

updateWeaponUsed :: HeroActionsState -> Weapon -> Weapon
updateWeaponUsed has w
  | heroActionsStateUseWeapon has && not current   = beingUsed w
  | not (heroActionsStateUseWeapon has) && current = notBeingUsed w
  | otherwise                                      = w
  where current = isUsingWeapon w

createHero :: Vector2 -> Hero
createHero pos = Hero pos 0 staticHeroActionsState (Pistol Nothing False)

createOriginHero :: Hero
createOriginHero = createHero (Vector2 0 0)

heroPosition :: Hero -> Vector2
heroPosition (Hero p _ _ _) = p

heroHeight :: Double
heroHeight = 1500

heroRotation :: Hero -> Double
heroRotation (Hero _ r _ _) = r

heroActionsState :: Hero -> HeroActionsState
heroActionsState (Hero _ _ a _) = a

heroWeapon :: Hero -> Weapon
heroWeapon (Hero _ _ _ w) = w

heroFieldOfViewSize :: Hero -> Double
heroFieldOfViewSize _ = pi / 3

heroLookRayAtFieldOfViewRatio :: Hero -> Double -> Ray
heroLookRayAtFieldOfViewRatio hero ratio = rotateRay (heroLookRay hero) rayRotation
  where
    fieldOfViewSize = heroFieldOfViewSize hero
    rayRotation = fieldOfViewSize * (ratio - 0.5)

heroLookRay :: Hero -> Ray
heroLookRay (Hero pos rot _ _) = createRay pos (Vector2 (sin rot) (cos rot))

moveHero :: Hero -> Double -> Hero
moveHero (Hero p r a w) m = Hero (p + (m |* angleToVector2 r)) r a w

updateHeroActionsState :: HeroActionsState -> Hero -> Hero
updateHeroActionsState a (Hero p r _ w) = Hero p r a w

updateHeroMoveDirection :: HeroActionsState -> Double
updateHeroMoveDirection s = forwardMovement + backwardMovement
  where
    forwardMovement = if heroActionsStateMoveForward s then 1 else 0
    backwardMovement = if heroActionsStateMoveBackward s then (-1) else 0

updateHeroRotation :: HeroActionsState -> Double
updateHeroRotation has = leftRotation + rightRotation
  where
    leftRotation = if heroActionsStateTurnLeft has then (-1) else 0
    rightRotation = if heroActionsStateTurnRight has then 1 else 0

-- TODO: Bound to (-pi) - pi
rotateHero :: Hero -> Double -> Hero
rotateHero (Hero p r a w) d = Hero p (r + d) a w

{-----------------------------------------------------------------------------------------------------------------------
 Environment
-----------------------------------------------------------------------------------------------------------------------}
data EnvItemType = Drum | Flag | Light
 deriving (Show, Eq, Ord)

data EnvItem = EnvItem EnvItemType Vector2
 deriving (Show, Eq)

instance SimEntity EnvItem where
 simUpdate _ _ = id

itemSize :: EnvItem -> Vector2
itemSize _ = Vector2 3000 3000

itemHeight :: Double
itemHeight = 3000

halfItemSize :: EnvItem -> Vector2
halfItemSize i = itemSize i *| 0.5

itemRectangle :: EnvItem -> Rectangle
itemRectangle i@(EnvItem _ o) = Rectangle (o - halfItemSize i) (itemSize i)

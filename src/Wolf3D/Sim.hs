module Wolf3D.Sim (
Wolf3DSimItem (SIEnvItem, SIHero),
  worldHero,
  worldHeroWeapon,
  worldEnvItems,
  worldEnvItemsTouching,
  updateWorldHeroActionsState,

  Hero,
  Weapon (Pistol),
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
  HeroAction (MoveForward, MoveBackward, TurnLeft, TurnRight, Shoot),
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


{-
 General
-}
data Wolf3DSimItem = SIEnvItem EnvItem | SIHero Hero
  deriving (Show, Eq)

instance SimItem Wolf3DSimItem where
  simUpdate t (SIEnvItem i) = SIEnvItem (simUpdate t i)
  simUpdate t (SIHero i) = SIHero (simUpdate t i)

worldHero :: World Wolf3DSimItem -> Hero
worldHero w = fromJust (fmap (\(SIHero h) -> h) (find (\i -> case i of (SIHero _) -> True; _ -> False) (worldItems w)))

worldHeroWeapon :: World Wolf3DSimItem -> Weapon
worldHeroWeapon = heroWeapon . worldHero

worldEnvItems :: World Wolf3DSimItem -> [EnvItem]
worldEnvItems w = map (\(SIEnvItem e) -> e) (filter (\i -> case i of (SIEnvItem _) -> True; _ -> False) (worldItems w))

updateWorldHeroActionsState :: World Wolf3DSimItem -> HeroActionsState -> World Wolf3DSimItem
updateWorldHeroActionsState w a = updateWorldHero w (\h -> updateHeroActionsState h a)

updateWorldHero :: World Wolf3DSimItem -> (Hero -> Hero) -> World Wolf3DSimItem
updateWorldHero w op = updateWorldItems w newItems
  where
    newItems = foldr foldStep [] (worldItems w)
    foldStep :: Wolf3DSimItem -> [Wolf3DSimItem] -> [Wolf3DSimItem]
    foldStep (SIHero h) accu = SIHero (op h) : accu
    foldStep i accu = i : accu

worldEnvItemsTouching :: Rectangle -> World Wolf3DSimItem -> [EnvItem]
worldEnvItemsTouching r w = filter (itemIsTouching r) (worldEnvItems w)

itemIsTouching :: Rectangle -> EnvItem -> Bool
itemIsTouching r i = rectangleOverlapsRectangle r (itemRectangle i)

{-
 Hero
-}
data Weapon = Pistol WorldTime
  deriving (Eq, Show)

--instance SimItem Weapon where
--  simUpdate m weapon = weapon
--    | canShoot world weapon = shoot world weapon
--    | otherwise             = weapon

--canShoot :: World Wolf3DSimIem -> Weapon -> Bool
--canShoot (World _ _ t) w@(Pistol lastShotTime) = t - lastShotTime >= timeBetweenShots w

-- TODO:
shoot :: Weapon -> Weapon
shoot = id

--timeBetweenShots :: Weapon -> Int
--timeBetweenShots (Pistol _) = 2000


data HeroActionsState = HeroActionsState
  { heroActionsStateMoveForward  :: Bool
  , heroActionsStateMoveBackward :: Bool
  , heroActionsStateTurnLeft     :: Bool
  , heroActionsStateTurnRight    :: Bool
  , heroActionsStateShoot        :: Bool
  }
  deriving (Show, Eq)

data HeroAction = MoveForward | MoveBackward | TurnLeft | TurnRight | Shoot

staticHeroActionsState :: HeroActionsState
staticHeroActionsState = HeroActionsState False False False False False

modifyHeroActionState :: HeroActionsState -> HeroAction -> Bool -> HeroActionsState
modifyHeroActionState (HeroActionsState _ d l r s) MoveForward a = HeroActionsState a d l r s
modifyHeroActionState (HeroActionsState u _ l r s) MoveBackward a = HeroActionsState u a l r s
modifyHeroActionState (HeroActionsState u d _ r s) TurnLeft a = HeroActionsState u d a r s
modifyHeroActionState (HeroActionsState u d l _ s) TurnRight a = HeroActionsState u d l a s
modifyHeroActionState (HeroActionsState u d l r _) Shoot a = HeroActionsState u d l r a

type Position = Vector2
type Rotation = Double
data Hero = Hero Position Rotation HeroActionsState Weapon
  deriving (Show, Eq)

instance SimItem Hero where
  simUpdate m h@(Hero _ _ has _) = tryAndShoot (rotateHero (moveHero h movement) rotation)
    where
      rotationDirection = updateHeroRotation has
      direction = updateHeroMoveDirection has
      heroMoveMetresPerSec = 8
      movement = direction * fromIntegral (m * heroMoveMetresPerSec)
      heroRotatePerMilli = 0.002
      rotation = rotationDirection * fromIntegral m * heroRotatePerMilli

tryAndShoot :: Hero -> Hero
tryAndShoot h@(Hero p r has w)
  | heroActionsStateShoot has = Hero p r has (shoot w)
  | otherwise                 = h

createHero :: Vector2 -> Hero
createHero pos = Hero pos 0 staticHeroActionsState (Pistol 0)

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

updateHeroActionsState :: Hero -> HeroActionsState -> Hero
updateHeroActionsState (Hero p r _ w) a = Hero p r a w

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

{- ---------------------
 Environment
 -}
data EnvItemType = Drum | Flag | Light
 deriving (Show, Eq, Ord)

data EnvItem = EnvItem EnvItemType Vector2
 deriving (Show, Eq)

instance SimItem EnvItem where
 simUpdate _ = id

itemSize :: EnvItem -> Vector2
itemSize _ = Vector2 3000 3000

itemHeight :: Double
itemHeight = 3000

halfItemSize :: EnvItem -> Vector2
halfItemSize i = itemSize i *| 0.5

itemRectangle :: EnvItem -> Rectangle
itemRectangle i@(EnvItem _ o) = Rectangle (o - halfItemSize i) (itemSize i)

module Wolf3D.Hero (
  Hero,
  createHero,
  heroPosition,
  heroRotation,
  heroLookRay,
  moveHero,
  rotateHero,
  heroHeight,
  HeroActionsState,
  staticHeroActionsState,
  modifyHeroActionState,
  HeroAction (MoveForward, MoveBackward, TurnLeft, TurnRight),
  heroActionsStateMoveForward,
  heroActionsStateMoveBackward,
  heroActionsStateTurnLeft,
  heroActionsStateTurnRight,
  updateHeroActionsState,
  heroActionsState,
  heroFieldOfViewSize,
  heroLookRayAtFieldOfViewRatio
) where

import Wolf3D.Geom
import Data.Vector


data HeroActionsState = HeroActionsState
  { heroActionsStateMoveForward  :: Bool
  , heroActionsStateMoveBackward :: Bool
  , heroActionsStateTurnLeft     :: Bool
  , heroActionsStateTurnRight    :: Bool
  }

data HeroAction = MoveForward | MoveBackward | TurnLeft | TurnRight

staticHeroActionsState :: HeroActionsState
staticHeroActionsState = HeroActionsState False False False False

modifyHeroActionState :: HeroActionsState -> HeroAction -> Bool -> HeroActionsState
modifyHeroActionState (HeroActionsState _ d l r) MoveForward a = HeroActionsState a d l r
modifyHeroActionState (HeroActionsState u _ l r) MoveBackward a = HeroActionsState u a l r
modifyHeroActionState (HeroActionsState u d _ r) TurnLeft a = HeroActionsState u d a r
modifyHeroActionState (HeroActionsState u d l _) TurnRight a = HeroActionsState u d l a

type Position = Vector2
type Rotation = Double
data Hero = Hero Position Rotation HeroActionsState

createHero :: Vector2 -> Hero
createHero pos = Hero pos 0 staticHeroActionsState

heroPosition :: Hero -> Vector2
heroPosition (Hero p _ _) = p

heroHeight :: Double
heroHeight = 1500

heroRotation :: Hero -> Double
heroRotation (Hero _ r _) = r

heroActionsState :: Hero -> HeroActionsState
heroActionsState (Hero _ _ a) = a

heroFieldOfViewSize :: Hero -> Double
heroFieldOfViewSize _ = pi / 3

heroLookRayAtFieldOfViewRatio :: Hero -> Double -> Ray
heroLookRayAtFieldOfViewRatio hero ratio = rotateRay (heroLookRay hero) rayRotation
  where
    fieldOfViewSize = heroFieldOfViewSize hero
    rayRotation = fieldOfViewSize * (ratio - 0.5)

heroLookRay :: Hero -> Ray
heroLookRay (Hero pos rot _) = createRay pos (Vector2 (sin rot) (cos rot))

moveHero :: Hero -> Double -> Hero
moveHero (Hero p r a) m = Hero (p + (m |* angleToVector2 r)) r a

updateHeroActionsState :: Hero -> HeroActionsState -> Hero
updateHeroActionsState (Hero p r _) = Hero p r

-- TODO: Bound to (-pi) - pi
rotateHero :: Hero -> Double -> Hero
rotateHero (Hero p r a) d = Hero p (r + d) a

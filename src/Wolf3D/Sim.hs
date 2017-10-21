module Wolf3D.Sim (tickWorld, tickWorldNTimes) where

import Wolf3D.World
import Wolf3D.Hero
import Wolf3D.Types

tickWorld :: PosInt -> World -> World
tickWorld timeStep world = advanceWorldTime movedWorld timeStep
  where
    hero = worldHero world
    movedHero = updateHero hero (heroActionsState hero) timeStep
    movedWorld = updateWorldHero world movedHero

updateHero :: Hero -> HeroActionsState -> PosInt -> Hero
updateHero h pas timeStep = rotateHero (moveHero h movement) rotation
  where
    rotationDirection = updateHeroRotation pas
    direction = updateHeroMoveDirection pas
    heroMoveMetresPerSec = 8
    movement = direction * fromIntegral (fromPosInt timeStep) * heroMoveMetresPerSec
    heroRotatePerMilli = 0.002
    rotation = rotationDirection * fromIntegral (fromPosInt timeStep) * heroRotatePerMilli

updateHeroMoveDirection :: HeroActionsState -> Double
updateHeroMoveDirection s = forwardMovement + backwardMovement
  where
    forwardMovement = if heroActionsStateMoveForward s then 1 else 0
    backwardMovement = if heroActionsStateMoveBackward s then (-1) else 0

updateHeroRotation :: HeroActionsState -> Double
updateHeroRotation pas = leftRotation + rightRotation
  where
    leftRotation = if heroActionsStateTurnLeft pas then (-1) else 0
    rightRotation = if heroActionsStateTurnRight pas then 1 else 0

tickWorldNTimes :: World -> PosInt -> PosZInt -> Maybe World
tickWorldNTimes w f n
  | n == posZInt0 = Nothing
  | otherwise     = Just (foldr foldStep w [1..(fromPosZInt n)])
    where
      foldStep :: Int -> World -> World
      foldStep _ = tickWorld f

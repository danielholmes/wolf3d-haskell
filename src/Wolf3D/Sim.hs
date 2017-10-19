module Wolf3D.Sim (tickWorld, tickWorldNTimes) where

import Data.Vector
import Wolf3D.Hero
import Wolf3D.World
import Wolf3D.Player
import Wolf3D.Types

tickWorld :: PosInt -> World -> World
tickWorld timeStep world = advanceWorldTime movedWorld timeStep
  where
    movedHero = updateHero (worldHero world) (worldPlayerActionsState world) timeStep
    movedWorld = updateWorldHero world movedHero

updateHero :: Hero -> PlayerActionsState -> PosInt -> Hero
updateHero h pas timeStep = rotateHero (moveHero h movement) rotation
  where
    rotationDirection = updateHeroRotation pas
    direction = updateHeroMoveDirection pas
    movement = direction *| fromIntegral (fromPosInt timeStep)
    rotation = rotationDirection * fromIntegral (fromPosInt timeStep) * 0.0001

updateHeroMoveDirection :: PlayerActionsState -> Vector2
updateHeroMoveDirection s = forwardMovement + backwardMovement
  where
    forwardMovement = if playerActionsStateMoveForward s then Vector2 0 1 else Vector2 0 0
    backwardMovement = if playerActionsStateMoveBackward s then Vector2 0 (-1) else Vector2 0 0

updateHeroRotation :: PlayerActionsState -> Double
updateHeroRotation pas = leftRotation + rightRotation
  where
    leftRotation = if playerActionsStateTurnLeft pas then (-1) else 0
    rightRotation = if playerActionsStateTurnLeft pas then 1 else 0

tickWorldNTimes :: World -> PosInt -> PosZInt -> Maybe World
tickWorldNTimes w f n
  | n == posZInt0 = Nothing
  | otherwise     = Just (foldr foldStep w [1..(fromPosZInt n)])
    where
      foldStep :: Int -> World -> World
      foldStep _ = tickWorld f

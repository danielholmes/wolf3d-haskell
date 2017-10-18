module Wolf3D.Sim (tickWorld, tickWorldNTimes) where

import Data.Vector
import Wolf3D.Hero
import Wolf3D.World
import Wolf3D.Player
import Wolf3D.Types

tickWorld :: PosInt -> World -> World
tickWorld timeStep world = advanceWorldTime movedWorld timeStep
  where
    pas = worldPlayerActionsState world
    direction = playerActionsStateToDirection pas
    movedHero = moveHero (worldHero world) (direction *| fromIntegral (fromPosInt timeStep))
    movedWorld = updateWorldHero world movedHero

playerActionsStateToDirection :: PlayerActionsState -> Vector2
playerActionsStateToDirection s = withDown
  where
    conditionalAdd :: Vector2 -> Vector2 -> Bool -> Vector2
    conditionalAdd p _ False = p
    conditionalAdd p d True = p + d

    withLeft = conditionalAdd (Vector2 0 0) (Vector2 (-1) 0) (playerActionsStateTurnLeft s)
    withRight = conditionalAdd withLeft (Vector2 1 0) (playerActionsStateTurnRight s)
    withUp = conditionalAdd withRight (Vector2 0 (-1)) (playerActionsStateUp s)
    withDown = conditionalAdd withUp (Vector2 0 1) (playerActionsStateDown s)

tickWorldNTimes :: World -> PosInt -> PosZInt -> Maybe World
tickWorldNTimes w f n
  | n == posZInt0 = Nothing
  | otherwise     = Just (foldr foldStep w [1..(fromPosZInt n)])
    where
      foldStep :: Int -> World -> World
      foldStep _ = tickWorld f

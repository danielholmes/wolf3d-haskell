module Wolf3D.Sim (tickWorld, tickWorldNTimes) where

import Wolf3D.World
import Wolf3D.Player
import Wolf3D.Types

tickWorld :: PosInt -> World -> World
tickWorld timeStep world = advanceWorldTime movedWorld timeStep
  where
    pas = worldPlayerActionsState world
    (dx,dy) = playerActionsStateToMovement pas
    movedWorld = moveWorld world (dx * fromPosInt timeStep, dy * fromPosInt timeStep)

playerActionsStateToMovement :: PlayerActionsState -> (Int,Int)
playerActionsStateToMovement s = withDown
  where
    conditionalAdd :: (Int, Int) -> (Int, Int) -> Bool -> (Int, Int)
    conditionalAdd p _ False = p
    conditionalAdd (x,y) (dx,dy) True = (x + dx, y + dy)

    withLeft = conditionalAdd (0,0) (-1,0) (playerActionsStateLeft s)
    withRight = conditionalAdd withLeft (1,0) (playerActionsStateRight s)
    withUp = conditionalAdd withRight (0,-1) (playerActionsStateUp s)
    withDown = conditionalAdd withUp (0,1) (playerActionsStateDown s)

tickWorldNTimes :: World -> PosInt -> PosZInt -> Maybe World
tickWorldNTimes w f n
  | n == posZInt0 = Nothing
  | otherwise     = Just (foldr foldStep w [1..(fromPosZInt n)])
    where
      foldStep :: Int -> World -> World
      foldStep _ = tickWorld f

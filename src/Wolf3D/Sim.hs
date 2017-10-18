module Wolf3D.Sim (tickWorld, tickWorldNTimes) where

import Wolf3D.World
import Wolf3D.Player
import Wolf3D.Types

tickWorld :: PosInt -> PositionWorld -> PositionWorld
tickWorld timeStep (PositionWorld (x,y) pas time) = PositionWorld (x+dx,y+dy) pas newTime
  where
    (dx,dy) = playerActionsStateToMovement pas
    newTime = posZInt (fromPosZInt time + fromPosInt timeStep)

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

tickWorldNTimes :: PositionWorld -> PosInt -> PosZInt -> Maybe PositionWorld
tickWorldNTimes w f n
  | n == posZInt0 = Nothing
  | otherwise     = Just (foldr foldStep w [1..(fromPosZInt n)])
    where
      foldStep :: Int -> PositionWorld -> PositionWorld
      foldStep _ = tickWorld f

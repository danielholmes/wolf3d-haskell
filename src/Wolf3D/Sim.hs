module Wolf3D.Sim (tickWorld, tickWorldNTimes) where

import Wolf3D.World
import Wolf3D.Player

tickWorld :: Int -> PositionWorld -> PositionWorld
tickWorld timeTick (PositionWorld (x,y) pas time) = PositionWorld (x+dx,y+dy) pas (time + timeTick)
  where (dx,dy) = playerActionsStateToMovement pas

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

tickWorldNTimes :: PositionWorld -> Int -> Int -> Maybe PositionWorld
tickWorldNTimes _ _ 0 = Nothing
tickWorldNTimes w f n = Just (foldr foldStep w [1..n])
  where
    foldStep :: Int -> PositionWorld -> PositionWorld
    foldStep _ = tickWorld f

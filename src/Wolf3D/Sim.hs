module Wolf3D.Sim (tickWorld) where

import Wolf3D.World

tickWorld :: Int -> PositionWorld -> PositionWorld
tickWorld timeTick (PositionWorld (x,y) d@(dx,dy) time) = PositionWorld (x+dx,y+dy) d (time + timeTick)

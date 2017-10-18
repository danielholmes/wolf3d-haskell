module Wolf3D.World (PositionWorld (PositionWorld), initWorld) where

data PositionWorld = PositionWorld (Int, Int) (Int, Int) Int

initWorld :: PositionWorld
initWorld = PositionWorld (0,0) (0,0) 0

module Wolf3D.World (World, createWorld, PositionWorld (PositionWorld), initWorld) where

newtype World = WorldImpl Double

createWorld :: World
createWorld = WorldImpl 0


data PositionWorld = PositionWorld (Int, Int) (Int, Int) Bool

initWorld :: PositionWorld
initWorld = PositionWorld (0,0) (0,0) False

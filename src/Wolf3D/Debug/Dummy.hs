module Wolf3D.Debug.Dummy (dummyWorld) where

import Wolf3D.World
import Wolf3D.Items
import Data.Vector


fromMetres :: Double -> Double
fromMetres m = m * 1000

fromBlocks :: Double -> Double
fromBlocks i = fromMetres (i * 3)

fromVBlocks :: Vector2 -> Vector2
fromVBlocks (Vector2 x y) = Vector2 (fromBlocks x) (fromBlocks y)

dummyWorld :: World
dummyWorld = createWorld walls items
  where
    metreWalls = [ Wall (Vector2 (-4) (-3)) (Vector2 0 7) Red
                 , Wall (Vector2 (-4) 4) (Vector2 3 0) Green
                 , Wall (Vector2 (-1) 4) (Vector2 0 6) Blue
                 , Wall (Vector2 (-1) 10) (Vector2 2 0) Red
                 , Wall (Vector2 1 10) (Vector2 0 (-6)) Green
                 , Wall (Vector2 1 4) (Vector2 3 0) Blue
                 , Wall (Vector2 4 4) (Vector2 0 (-7)) Red]
    walls = map (\(Wall o s m) -> Wall (fromVBlocks o) (fromVBlocks s) m) metreWalls
    items = [Item Drum (fromVBlocks (Vector2 (-3.5) 3.5))]

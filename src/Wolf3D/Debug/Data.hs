module Wolf3D.Debug.Data (dummyWorld) where

import Wolf3D.World
import Data.Vector


fromMetres :: Double -> Double
fromMetres m = m * 1000

fromVMetres :: Vector2 -> Vector2
fromVMetres (Vector2 x y) = Vector2 (fromMetres x) (fromMetres y)

dummyWorld :: World
dummyWorld = createWorld walls
  where
    metreWalls = [ Wall (Vector2 (-10) (-10)) (Vector2 0 20) Red
                 , Wall (Vector2 (-10) 10) (Vector2 9 0) Green
                 , Wall (Vector2 (-1) 10) (Vector2 0 10) Blue
                 , Wall (Vector2 (-1) 20) (Vector2 2 0) Red
                 , Wall (Vector2 1 20) (Vector2 0 (-10)) Green
                 , Wall (Vector2 1 10) (Vector2 9 0) Blue
                 , Wall (Vector2 10 10) (Vector2 0 (-20)) Red]
    walls = map (\(Wall o s m) -> Wall (fromVMetres o) (fromVMetres s) m) metreWalls

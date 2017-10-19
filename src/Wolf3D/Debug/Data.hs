module Wolf3D.Debug.Data (dummyWorld) where

import Wolf3D.World
import Data.Vector


dummyWorld :: World
dummyWorld = createWorld walls
  where
    walls = [ Wall (Vector2 (-1000) (-1000)) (Vector2 0 2000) Red
            , Wall (Vector2 (-1000) 1000) (Vector2 950 0) Green
            , Wall (Vector2 (-50) 1000) (Vector2 0 1000) Blue
            , Wall (Vector2 (-50) 2000) (Vector2 100 0) Red
            , Wall (Vector2 50 2000) (Vector2 0 (-1000)) Green
            , Wall (Vector2 50 1000) (Vector2 950 0) Blue
            , Wall (Vector2 1000 1000) (Vector2 0 (-2000)) Red]

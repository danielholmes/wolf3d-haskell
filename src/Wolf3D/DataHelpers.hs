module Wolf3D.DataHelpers (visualListToWallMap) where

import Data.Array
import Wolf3D.Engine

visualListToWallMap :: [[Maybe WallMaterial]] -> WallMap
visualListToWallMap wm = array ((0, 0), (cols - 1, rows - 1)) items
  where
    rows = length wm
    cols = length (wm!!0)
    items :: [((Int, Int), Maybe WallMaterial)]
    items = concat (map (\(y, row) -> map (\(x, cell) -> ((x, y), cell)) (zip [0..] row)) (zip [0..] wm))

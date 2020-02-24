module Wolf3D.Debug.Dummy (
  dummyWorld,
  dummyWorldSingleWall
) where

import Wolf3D.Engine
import Wolf3D.Sim
import Data.Maybe

dummyWorld :: World Wolf3DSimEntity
dummyWorld = fromGrid GreyCeiling [["WB1", "WB1", "WG1", "WB1", "WB1"],
                                    ["WB1", "DR",  "",    "DR",  "WB1"],
                                    ["WB1", "",    "",    "",    "WB1"],
                                    ["WB2", "",    "H",   "",    "WB2"],
                                    ["WB2", "",    "",    "",    "WB2"],
                                    ["WB2", "WB2", "WB2", "WB2", "WB2"]]

dummyWorldSingleWall :: World Wolf3DSimEntity
dummyWorldSingleWall = createWorld GreyCeiling [] [SEHero (createHeroFromTilePosition (1, 1))]
--  where
--    wall = Wall (Vector2 (-24000) 9000) (Vector2 44000 0) Blue

fromGrid :: Ceiling -> [[String]] -> World Wolf3DSimEntity
fromGrid c rows = createWorld c ws is
  where
    wm = emptyWallMap (length rows) (length (head rows))
    (ws, is) = fromGridRows wm 0 rows

mapSet :: WallMap -> TileCoord -> WallMaterial -> WallMap
mapSet wm (x, y) w = colsBefore ++ [newCol] ++ colsAfter
  where
    (colsBefore, col:colsAfter) = splitAt x wm
    (cellsBefore, _:cellsAfter) = splitAt y col
    newCol = cellsBefore ++ [Just w] ++ cellsAfter

--    foldStep :: Wall -> WallMap -> WallMap
--    foldStep (Wall p _ m) accu = mapSet accu (globalPosToTileCoord p) m
--
--    numRows = (globalSizeToTileCoord (maximum (map (\(Wall (Vector2 x _) _ _) -> x) ws))) + 1
--    numCols = (globalSizeToTileCoord (maximum (map (\(Wall (Vector2 _ y) _ _) -> y) ws))) + 1
--
--    createWM = foldr foldStep (emptyWallMap numCols numRows) ws

fromGridRows :: WallMap -> Int -> [[String]] -> (WallMap, [Wolf3DSimEntity])
fromGridRows wm _ [] = (wm, [])
fromGridRows wm y (r:rs) = (nextWM, rowIs ++ nextIs)
  where
    (rowWM, rowIs) = fromGridRow wm (0, y) r
    (nextWM, nextIs) = fromGridRows rowWM (y + 1) rs

fromGridRow :: WallMap -> (Int, Int) -> [String] -> (WallMap, [Wolf3DSimEntity])
fromGridRow wm _ [] = (wm, [])
fromGridRow wm (x, y) (c:cs) = (nextWM, (maybeToList cellIs) ++ nextIs)
  where
    (cellWM, cellIs) = fromGridCell wm (x, y) c
    (nextWM, nextIs) = fromGridRow cellWM (x+1, y) cs

fromGridCell :: WallMap -> TileCoord -> String -> (WallMap, Maybe Wolf3DSimEntity)
fromGridCell wm _ "" = (wm, Nothing)
fromGridCell wm pos "H" = (wm, Just (SEHero (rotateHero (createHeroFromTilePosition pos) ((-90) * 20))))
fromGridCell wm pos "DR" = (wm, Just (SEEnvItem (EnvItem Drum (tileCoordToCentreGlobalPos pos))))
fromGridCell wm t "WB1" = (mapSet wm t Blue1, Nothing)
fromGridCell wm t "WB2" = (mapSet wm t Blue2, Nothing)
fromGridCell wm t "WG1" = (mapSet wm t Grey1, Nothing)
fromGridCell wm t "WG2" = (mapSet wm t Grey2, Nothing)
fromGridCell _ _ c = error ("Unknown cell '" ++ c ++ "'")

--createWalls :: TileCoord -> WallMaterial -> [Wall]
--createWalls pos@(x, y) m = [top, right, bottom, left]
--  where
--    top = Wall (tileCoordToGlobalPos pos) (tileCoordToGlobalPos (1, 0)) m
--    right = Wall (tileCoordToGlobalPos (x + 1, y)) (tileCoordToGlobalPos (0, 1)) m
--    bottom = Wall (tileCoordToGlobalPos (x, y + 1)) (tileCoordToGlobalPos (1, 0)) m
--    left = Wall (tileCoordToGlobalPos pos) (tileCoordToGlobalPos (0, 1)) m

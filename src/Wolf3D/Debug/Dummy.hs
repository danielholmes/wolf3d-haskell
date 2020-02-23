module Wolf3D.Debug.Dummy (
  dummyWorld,
  dummyWorld2,
  dummyWorldSingleWall
) where

import Wolf3D.Engine
import Wolf3D.Sim
import Data.Vector
import Data.Maybe


fromMetres :: Double -> Double
fromMetres m = m * 10000

fromBlocks :: Double -> Double
fromBlocks i = fromMetres (i * 3)

fromVBlocks :: Vector2 -> Vector2
fromVBlocks (Vector2 x y) = Vector2 (fromBlocks x) (fromBlocks y)

dummyWorld :: World Wolf3DSimEntity
dummyWorld = createWorld GreenCeiling wm items
  where
--    metreWalls = [ Wall (Vector2 0 0) (Vector2 0 7) Red
--                 , Wall (Vector2 (-4) 4) (Vector2 3 0) Green
--                 , Wall (Vector2 (-1) 4) (Vector2 0 6) Blue
--                 , Wall (Vector2 (-1) 10) (Vector2 2 0) Red
--                 , Wall (Vector2 1 10) (Vector2 0 (-6)) Green
--                 , Wall (Vector2 1 4) (Vector2 3 0) Blue
--                 , Wall (Vector2 4 4) (Vector2 0 (-7)) Red]
--    walls = map (\(Wall o s m) -> Wall (fromVBlocks o) (fromVBlocks s) m) metreWalls
    wm = []
    heroPos = (5, 5)
    items = [ SEEnvItem (EnvItem Drum (fromVBlocks (Vector2 (-3.5) 3.5)))
            , SEEnvItem (EnvItem Light (fromVBlocks (Vector2 0 2)))
            , SEEnvItem (EnvItem Flag (fromVBlocks (Vector2 3.5 3.5)))
            , SEHero (createHeroFromTilePosition heroPos) ]

dummyWorld2 :: World Wolf3DSimEntity
dummyWorld2 = fromGrid YellowCeiling [["WB1", "WB1", "WB1", "WB2", "WB2"],
                                      ["WB2", "DR",  "",    "DR",  "WB1"],
                                      ["WB1", "",    "",    "",    "WB1"],
                                      ["WB4", "",    "H",   "",    "WB2"],
                                      ["WB1", "",    "",    "",    "WB2"],
                                      ["WB2", "WB1", "WB1", "WB1", "WB2"]]

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
fromGridCell wm t "WB1" = (mapSet wm t Blue, Nothing)
fromGridCell wm t "WB2" = (mapSet wm t Blue2, Nothing)
fromGridCell wm t "WB3" = (mapSet wm t Blue3, Nothing)
fromGridCell wm t "WB4" = (mapSet wm t Blue4, Nothing)
fromGridCell _ _ c = error ("Unknown cell '" ++ c ++ "'")

--createWalls :: TileCoord -> WallMaterial -> [Wall]
--createWalls pos@(x, y) m = [top, right, bottom, left]
--  where
--    top = Wall (tileCoordToGlobalPos pos) (tileCoordToGlobalPos (1, 0)) m
--    right = Wall (tileCoordToGlobalPos (x + 1, y)) (tileCoordToGlobalPos (0, 1)) m
--    bottom = Wall (tileCoordToGlobalPos (x, y + 1)) (tileCoordToGlobalPos (1, 0)) m
--    left = Wall (tileCoordToGlobalPos pos) (tileCoordToGlobalPos (0, 1)) m

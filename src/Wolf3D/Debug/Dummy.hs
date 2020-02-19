module Wolf3D.Debug.Dummy (
  dummyWorld,
  dummyWorld2,
  dummyWorldSingleWall
) where

import Wolf3D.Engine
import Wolf3D.Sim
import Data.Vector


fromMetres :: Double -> Double
fromMetres m = m * 10000

fromBlocks :: Double -> Double
fromBlocks i = fromMetres (i * 3)

fromVBlocks :: Vector2 -> Vector2
fromVBlocks (Vector2 x y) = Vector2 (fromBlocks x) (fromBlocks y)

dummyWorld :: World Wolf3DSimEntity
dummyWorld = createWorld GreenCeiling walls items
  where
    metreWalls = [ Wall (Vector2 0 0) (Vector2 0 7) Red
                 , Wall (Vector2 (-4) 4) (Vector2 3 0) Green
                 , Wall (Vector2 (-1) 4) (Vector2 0 6) Blue
                 , Wall (Vector2 (-1) 10) (Vector2 2 0) Red
                 , Wall (Vector2 1 10) (Vector2 0 (-6)) Green
                 , Wall (Vector2 1 4) (Vector2 3 0) Blue
                 , Wall (Vector2 4 4) (Vector2 0 (-7)) Red]
    walls = map (\(Wall o s m) -> Wall (fromVBlocks o) (fromVBlocks s) m) metreWalls
    heroPos = (5, 5)
    items = [ SEEnvItem (EnvItem Drum (fromVBlocks (Vector2 (-3.5) 3.5)))
            , SEEnvItem (EnvItem Light (fromVBlocks (Vector2 0 2)))
            , SEEnvItem (EnvItem Flag (fromVBlocks (Vector2 3.5 3.5)))
            , SEHero (createHeroFromTilePosition heroPos) ]

dummyWorld2 :: World Wolf3DSimEntity
dummyWorld2 = fromGrid YellowCeiling [["WB1", "WB1", "WB1", "WB2", "WB2"],
                                      ["WB2", "DR",  "",    "DR",  "WB1"],
                                      ["WB1", "",    "",    "",    "WB1"],
                                      ["WB1", "",    "",    "",    "WB1"],
                                      ["WB2", "",    "",    "",    "WB1"],
                                      ["WB2", "",    "",    "",    "WB2"],
                                      ["WB1", "",    "",    "",    "WB2"],
                                      ["WB2", "",    "H",   "",    "WB2"],
                                      ["WB2", "",    "",    "",    "WB1"],
                                      ["WB1", "",    "",    "",    "WB1"],
                                      ["WB1", "",    "",    "",    "WB2"]]

dummyWorldSingleWall :: World Wolf3DSimEntity
dummyWorldSingleWall = createWorld GreyCeiling [wall] [SEHero (createHeroFromTilePosition (1, 1))]
  where
    wall = Wall (Vector2 (-24000) 9000) (Vector2 44000 0) Blue

fromGrid :: Ceiling -> [[String]] -> World Wolf3DSimEntity
fromGrid c rows = createWorld c ws is
  where
    (ws, is) = fromGridRows 0 rows

fromGridRows :: Int -> [[String]] -> ([Wall], [Wolf3DSimEntity])
fromGridRows _ [] = ([], [])
fromGridRows y (r:rs) = (rowWs ++ nextWs, rowIs ++ nextIs)
  where
    (rowWs, rowIs) = fromGridRow (0, y) r
    (nextWs, nextIs) = fromGridRows (y-1) rs

fromGridRow :: (Int, Int) -> [String] -> ([Wall], [Wolf3DSimEntity])
fromGridRow _ [] = ([], [])
fromGridRow (x, y) (c:cs) = (cellWs ++ nextWs, cellIs ++ nextIs)
  where
    (cellWs, cellIs) = fromGridCell (x, y) c
    (nextWs, nextIs) = fromGridRow (x+1, y) cs

fromGridCell :: TileCoord -> String -> ([Wall], [Wolf3DSimEntity])
fromGridCell _ "" = ([], [])
fromGridCell t "H" = ([], [SEHero (createHeroFromTilePosition t)])
fromGridCell pos "DR" = ([], [SEEnvItem (EnvItem Drum (tileCoordToCentreGlobalPos pos))])
fromGridCell t "WB1" = (createWalls t Blue, [])
fromGridCell t "WB2" = (createWalls t Blue2, [])
fromGridCell pos "WB3" = (createWalls pos Blue3, [])
fromGridCell pos "WB4" = (createWalls pos Blue4, [])
fromGridCell _ c = error ("Unknown cell '" ++ c ++ "'")

createWalls :: TileCoord -> WallMaterial -> [Wall]
createWalls pos@(x, y) m = [top, right, bottom, left]
  where
    top = Wall (tileCoordToGlobalPos pos) (tileCoordToGlobalPos (1, 0)) m
    right = Wall (tileCoordToGlobalPos (x + 1, y)) (tileCoordToGlobalPos (0, (-1))) m
    bottom = Wall (tileCoordToGlobalPos (x, y - 1)) (tileCoordToGlobalPos (1, 0)) m
    left = Wall (tileCoordToGlobalPos pos) (tileCoordToGlobalPos (0, (-1))) m

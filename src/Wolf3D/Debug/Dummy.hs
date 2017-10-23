module Wolf3D.Debug.Dummy (
  dummyWorld,
  dummyWorld2,
  dummyWorldSingleWall
) where

import SimEngine.Engine
import Wolf3D.Sim
import Data.Vector


fromMetres :: Double -> Double
fromMetres m = m * 1000

fromBlocks :: Double -> Double
fromBlocks i = fromMetres (i * 3)

fromVBlocks :: Vector2 -> Vector2
fromVBlocks (Vector2 x y) = Vector2 (fromBlocks x) (fromBlocks y)

dummyWorld :: World Wolf3DSimEntity
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
    items = [ SEEnvItem (EnvItem Drum (fromVBlocks (Vector2 (-3.5) 3.5)))
            , SEEnvItem (EnvItem Light (fromVBlocks (Vector2 0 2)))
            , SEEnvItem (EnvItem Flag (fromVBlocks (Vector2 3.5 3.5)))
            , SEHero createOriginHero ]

dummyWorld2 :: World Wolf3DSimEntity
dummyWorld2 = fromGrid [["WB1", "WB1", "WB1", "WB2", "WB2"],
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
dummyWorldSingleWall = createWorld [wall] [SEHero createOriginHero]
  where
    wall = Wall (Vector2 (-24000) 9000) (Vector2 44000 0) Blue

fromGrid :: [[String]] -> World Wolf3DSimEntity
fromGrid rows = createWorld ws is
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
    coordPos = Vector2 (fromIntegral x) (fromIntegral y)
    (cellWs, cellIs) = fromGridCell coordPos c
    (nextWs, nextIs) = fromGridRow (x+1, y) cs

fromGridCell :: Vector2 -> String -> ([Wall], [Wolf3DSimEntity])
fromGridCell _ "" = ([], [])
fromGridCell pos "H" = ([], [SEHero (createHero (fromVBlocks pos))])
fromGridCell pos "DR" = ([], [SEEnvItem (EnvItem Drum (toItemPos pos))])
fromGridCell pos "WB1" = (createWalls pos Blue, [])
fromGridCell pos "WB2" = (createWalls pos Blue2, [])
fromGridCell pos "WB3" = (createWalls pos Blue3, [])
fromGridCell pos "WB4" = (createWalls pos Blue4, [])
fromGridCell _ c = error ("Unknown cell '" ++ c ++ "'")

createWalls :: Vector2 -> WallMaterial -> [Wall]
createWalls pos m = [top, right, bottom, left]
  where
    downSize = fromVBlocks (Vector2 0 (-1))
    top = Wall (fromVBlocks pos) (fromVBlocks (Vector2 1 0)) m
    right = Wall (fromVBlocks (pos + Vector2 1 0)) downSize m
    bottom = Wall (fromVBlocks (pos + Vector2 0 (-1))) (fromVBlocks (Vector2 1 0)) m
    left = Wall (fromVBlocks pos) downSize m

toItemPos :: Vector2 -> Vector2
toItemPos pos = fromVBlocks (pos + Vector2 0.5 (-0.5))

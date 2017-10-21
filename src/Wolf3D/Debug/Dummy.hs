module Wolf3D.Debug.Dummy (dummyWorld, dummyWorld2) where

import Wolf3D.World
import Wolf3D.Items
import Data.Vector
import Control.Applicative


fromMetres :: Double -> Double
fromMetres m = m * 1000

fromBlocks :: Double -> Double
fromBlocks i = fromMetres (i * 3)

fromVBlocks :: Vector2 -> Vector2
fromVBlocks (Vector2 x y) = Vector2 (fromBlocks x) (fromBlocks y)

dummyWorld :: World
dummyWorld = createWorld walls items Nothing
  where
    metreWalls = [ Wall (Vector2 (-4) (-3)) (Vector2 0 7) Red
                 , Wall (Vector2 (-4) 4) (Vector2 3 0) Green
                 , Wall (Vector2 (-1) 4) (Vector2 0 6) Blue
                 , Wall (Vector2 (-1) 10) (Vector2 2 0) Red
                 , Wall (Vector2 1 10) (Vector2 0 (-6)) Green
                 , Wall (Vector2 1 4) (Vector2 3 0) Blue
                 , Wall (Vector2 4 4) (Vector2 0 (-7)) Red]
    walls = map (\(Wall o s m) -> Wall (fromVBlocks o) (fromVBlocks s) m) metreWalls
    items = [ Item Drum (fromVBlocks (Vector2 (-3.5) 3.5))
            , Item Light (fromVBlocks (Vector2 0 2))
            , Item Flag (fromVBlocks (Vector2 3.5 3.5))]

dummyWorld2 :: World
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

fromGrid :: [[String]] -> World
fromGrid rows = createWorld ws is pos
  where
    (ws, is, pos) = fromGridRows 0 rows

fromGridRows :: Int -> [[String]] -> ([Wall], [Item], Maybe Vector2)
fromGridRows _ [] = ([], [], Nothing)
fromGridRows y (r:rs) = (rowWs ++ nextWs, rowIs ++ nextIs, pos <|> nextPos)
  where
    (rowWs, rowIs, pos) = fromGridRow (0, y) r
    (nextWs, nextIs, nextPos) = fromGridRows (y-1) rs

fromGridRow :: (Int, Int) -> [String] -> ([Wall], [Item], Maybe Vector2)
fromGridRow _ [] = ([], [], Nothing)
fromGridRow (x, y) (c:cs) = (cellWs ++ nextWs, cellIs ++ nextIs, pos <|> nextPos)
  where
    coordPos = Vector2 (fromIntegral x) (fromIntegral y)
    (cellWs, cellIs, pos) = fromGridCell coordPos c
    (nextWs, nextIs, nextPos) = fromGridRow (x+1, y) cs

fromGridCell :: Vector2 -> String -> ([Wall], [Item], Maybe Vector2)
fromGridCell _ "" = ([], [], Nothing)
fromGridCell pos "H" = ([], [], Just (fromVBlocks pos))
fromGridCell pos "DR" = ([], [Item Drum (toItemPos pos)], Nothing)
fromGridCell pos "WB1" = (createWalls pos Blue, [], Nothing)
fromGridCell pos "WB2" = (createWalls pos Blue2, [], Nothing)
fromGridCell pos "WB3" = (createWalls pos Blue3, [], Nothing)
fromGridCell pos "WB4" = (createWalls pos Blue4, [], Nothing)
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

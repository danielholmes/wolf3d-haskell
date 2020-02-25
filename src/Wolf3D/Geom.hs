module Wolf3D.Geom (
  Rectangle (Rectangle),
  Line,
  rotateVector2,
  rectangleOverlapsRectangle,
  degToRad
) where

import Data.Vector


type Line = (Vector2, Vector2)

data Rectangle = Rectangle Vector2 Vector2

degToRad :: Double
degToRad = pi / 180

rotateVector2 :: Vector2 -> Double -> Vector2
rotateVector2 (Vector2 x y) r = Vector2 newX newY
  where
    mr = -r
    s = sin mr
    c = cos mr
    newX = (c * x) - (s * y)
    newY = (s * x) + (c * y)

rectangleOverlapsRectangle :: Rectangle -> Rectangle -> Bool
rectangleOverlapsRectangle (Rectangle (Vector2 x1 y1) (Vector2 w1 h1)) (Rectangle (Vector2 x2 y2) (Vector2 w2 h2)) =
  x1 < (x2 + w2) && (x1 + w1) > x2 && y1 < (y2 + h2) && (y1 + h1) > y2

module Wolf3D.SpecHelp (
  veryCloseToVector2,
  veryCloseToDouble
) where

import Data.Vector


veryCloseToVector2 :: Vector2 -> Vector2 -> Bool
veryCloseToVector2 (Vector2 x1 y1) (Vector2 x2 y2) = veryCloseToDouble x1 x2 && veryCloseToDouble y1 y2

veryCloseToDouble :: Double -> Double -> Bool
veryCloseToDouble d1 d2 = abs (d1 - d2) < 0.00001

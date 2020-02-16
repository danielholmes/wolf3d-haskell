module Wolf3D.SpecHelp (
  veryCloseToRay,
  veryCloseToVector2,
  veryCloseToDouble
) where

import Wolf3D.Geom
import Data.Vector


veryCloseToRay :: Ray -> Ray -> Bool
veryCloseToRay r1 r2 = veryCloseToVector2 o1 o2 && veryCloseToVector2 d1 d2
  where
    o1 = rayOrigin r1
    d1 = rayDirection r1
    o2 = rayOrigin r2
    d2 = rayDirection r2

veryCloseToVector2 :: Vector2 -> Vector2 -> Bool
veryCloseToVector2 (Vector2 x1 y1) (Vector2 x2 y2) = veryCloseToDouble x1 x2 && veryCloseToDouble y1 y2

veryCloseToDouble :: Double -> Double -> Bool
veryCloseToDouble d1 d2 = abs (d1 - d2) < 0.00001

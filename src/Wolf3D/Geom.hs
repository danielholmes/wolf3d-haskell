module Wolf3D.Geom (
  Ray,
  Rectangle (Rectangle),
  createRay,
  rayOrigin,
  rayDirection,
  moveRayAlongDirection,
  rotateRay,
  angleToVector2
) where

import Data.Vector


data Rectangle = Rectangle Vector2 Vector2

data Ray = Ray Vector2 Vector2

instance Show Ray where
  show (Ray o d) = "Ray " ++ show o ++ " " ++ show d

instance Eq Ray where
  (==) (Ray o1 d1) (Ray o2 d2) = o1 == o2 && d1 == d2

createRay :: Vector2 -> Vector2 -> Ray
createRay p m
  | vmag m > 0 = Ray p (vnormalise m)
  | otherwise  = error ("Invalid ray " ++ show p ++ " " ++ show m)

rayOrigin :: Ray -> Vector2
rayOrigin (Ray o _) = o

rayDirection :: Ray -> Vector2
rayDirection (Ray _ d) = d

moveRayAlongDirection :: Ray -> Double -> Ray
moveRayAlongDirection (Ray p d) m = Ray (p + (m |* d)) d

rotateRay :: Ray -> Double -> Ray
rotateRay (Ray o d) a = Ray o (rotateVector2 d a)

rotateVector2 :: Vector2 -> Double -> Vector2
rotateVector2 (Vector2 x y) r = Vector2 newX newY
  where
    mr = -r
    s = sin mr
    c = cos mr
    newX = (c * x) - (s * y)
    newY = (s * x) + (c * y)

angleToVector2 :: Double -> Vector2
angleToVector2 a = Vector2 (sin a) (cos a)

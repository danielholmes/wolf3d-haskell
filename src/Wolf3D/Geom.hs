module Wolf3D.Geom (
  Ray,
  createRay,
  rayOrigin,
  rayDirection
) where

import Data.Vector


data Ray = Ray Vector2 Vector2

instance Show Ray where
  show (Ray o d) = "Ray " ++ show o ++ " " ++ show d

instance Eq Ray where
  (==) (Ray o1 d1) (Ray o2 d2) = o1 == o2 && d1 == d2

createRay :: Vector2 -> Vector2 -> Ray
createRay p m
  | vmag m > 0 = Ray p m
  | otherwise  = error ("Invalid ray " ++ show p ++ " " ++ show m)

rayOrigin :: Ray -> Vector2
rayOrigin (Ray o _) = o

rayDirection :: Ray -> Vector2
rayDirection (Ray _ d) = d

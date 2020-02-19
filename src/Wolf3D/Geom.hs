module Wolf3D.Geom (
  Ray,
  Rectangle (Rectangle),
  Line,
  createRay,
  rayOrigin,
  rayDirection,
  moveRayAlongDirection,
  rotateRay,
  rayAngle,
  angleToVector2,
  vector2ToAngle,
  rotateVector2,
  rectangleTouchesLine,
  rayLineIntersection,
  vcross,
  vectorDist,
  rectangleSides,
  rectangleOverlapsRectangle,
  rectMult,
  rectAdd,
  deg2Rad
) where

import Data.Vector
import Data.Maybe


type Line = (Vector2, Vector2)

data Rectangle = Rectangle Vector2 Vector2

data Ray = Ray Vector2 Vector2
  deriving (Show, Eq)

--data Angle = Angle Ray Ray Double
--
--createAngleFromCentre :: Ray -> Double -> Angle
--createAngleFromCentre centre size = Angle (rotateRay centre (-halfSize)) (rotateRay centre halfSize) size
--  where halfSize = size / 2

deg2Rad :: Double
deg2Rad = pi / 180

createRay :: Vector2 -> Vector2 -> Ray
createRay p m
  | vmag m > 0 = Ray p (vnormalise m)
  | otherwise  = error ("Invalid ray " ++ show p ++ " " ++ show m)

rayAngle :: Ray -> Double
rayAngle (Ray _ m) = vector2ToAngle m

rayOrigin :: Ray -> Vector2
rayOrigin (Ray o _) = o

rayDirection :: Ray -> Vector2
rayDirection (Ray _ d) = d

moveRayAlongDirection :: Ray -> Double -> Ray
moveRayAlongDirection (Ray p d) m = Ray (p + (m |* d)) d

rotateRay :: Ray -> Double -> Ray
rotateRay (Ray o d) a = Ray o (rotateVector2 d a)

-- https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect/565282#565282
-- https://rootllama.wordpress.com/2014/06/20/ray-line-segment-intersection-test-in-2d/
rayLineIntersection :: Ray -> Line -> Maybe Vector2
rayLineIntersection ray (q, s)
  | rxs == 0 && qmpxr == 0                 = Nothing -- Collinear
  | rxs == 0 && qmpxr /= 0                 = Nothing -- Parallel
  | rxs /= 0 && t >= 0 && u >= 0 && u <= 1 = Just intersectP
  | otherwise                              = Nothing
  where
    p = rayOrigin ray
    r = rayDirection ray
    rxs = vcross2 r s
    qmp = q - p
    qmpxr = vcross2 qmp r
    u = vcross2 qmp r / rxs
    t = vcross2 qmp s / rxs
    intersectP = p + (t |* r)

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

vector2ToAngle :: Vector2 -> Double
vector2ToAngle (Vector2 x y) = atan2 x y

vectorDist :: Vector2 -> Vector2 -> Double
vectorDist (Vector2 x1 y1) (Vector2 x2 y2) = sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2))

vcross2 :: Vector2 -> Vector2 -> Scalar
vcross2 (Vector2 x1 y1) (Vector2 x2 y2) = (x1 * y2) - (y1 * x2)

lineLineIntersection :: Line -> Line -> Maybe Vector2
lineLineIntersection (p, r) (q, s)
  | rxs == 0 && qmpxr == 0                           = Nothing -- Collinear
  | rxs == 0 && qmpxr /= 0                           = Nothing -- Parallel
  | rxs /= 0 && t >= 0 && t <= 1 && u >= 0 && u <= 1 = Just intersectP
  | otherwise                                        = Nothing
  where
    rxs = vcross2 r s
    qmp = q - p
    qmpxr = vcross2 qmp r
    u = vcross2 qmp r / rxs
    t = vcross2 qmp s / rxs
    intersectP = p + (t |* r)

rectangleTouchesLine :: Rectangle -> Line -> Bool
rectangleTouchesLine r l@(o, s) = touches || containsBoth
  where
    touches = any (isJust . lineLineIntersection l) (rectangleSides r)
    containsBoth = rectangleContainsPoint r o && rectangleContainsPoint r (o + s)

rectangleContainsPoint :: Rectangle -> Vector2 -> Bool
rectangleContainsPoint (Rectangle (Vector2 ox oy) (Vector2 w h)) (Vector2 px py) = horizontalInside && verticalInside
  where
    horizontalInside = px >= ox && px <= (ox + w)
    verticalInside = py >= oy && py <= (oy + h)

rectangleSides :: Rectangle -> [Line]
rectangleSides (Rectangle topLeft (Vector2 w h)) = [top, right, bottom, left]
  where
    horizontalMagnitude = Vector2 w 0
    verticalMagnitude = Vector2 0 h
    topRight = topLeft + horizontalMagnitude
    bottomLeft = topLeft + verticalMagnitude
    top = (topLeft, horizontalMagnitude)
    bottom = (bottomLeft, horizontalMagnitude)
    right = (topRight, verticalMagnitude)
    left = (topLeft, verticalMagnitude)

rectangleOverlapsRectangle :: Rectangle -> Rectangle -> Bool
rectangleOverlapsRectangle (Rectangle (Vector2 x1 y1) (Vector2 w1 h1)) (Rectangle (Vector2 x2 y2) (Vector2 w2 h2)) =
  x1 < (x2 + w2) && (x1 + w1) > x2 && y1 < (y2 + h2) && (y1 + h1) > y2

rectMult :: Rectangle -> Double -> Rectangle
rectMult (Rectangle o s) scale = Rectangle (o *| scale) (s *| scale)

rectAdd :: Rectangle -> Vector2 -> Rectangle
rectAdd (Rectangle o s) d = Rectangle (o + d) s

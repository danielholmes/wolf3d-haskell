module Wolf3D.Display.Ray (
  castRay,
  Hit (Hit, material)
) where

import Data.Vector
import Wolf3D.Engine
import Wolf3D.Sim
import Wolf3D.Geom
import Data.Bits
import Debug.Trace

data Hit = Hit {material :: WallMaterial}
  deriving (Eq, Show)

-- Taken from original source, don't know exact meaning yet
focalLength :: Double
focalLength = 0x5700

--sinTable :: [Double]
--sinTable = map (\a -> (fromIntegral tileGlobalSize) * sin (a * deg2Rad)) [0..(360 - 1)]
--
--cosTable :: [Double]
--cosTable = map (\a -> (fromIntegral tileGlobalSize) * cos (a * deg2Rad)) [0..(360 - 1)]

castRay :: WallMap -> Vector2 -> SnappedRotation -> Maybe Hit
castRay [] _ _   = Nothing
-- (c:cs)
castRay wm (Vector2 x y) viewAngle = traceShow ((focalXI, focalYI)
                                              , (viewTileX, viewTileY)
                                              , (focalTileX, focalTileY)
                                              , (xPartial, yPartial)
                                              , (xStep, yStep)
                                              , (xTileStep, yTileStep)
                                              , horIntercept
                                              , verIntercept) (if verDist > horDist then horRayCheck wm horIntercept verIntercept xStep yStep else verRayCheck wm horIntercept verIntercept xStep yStep)
  where
    angleRad = fromIntegral viewAngle * deg2Rad
    viewSin = sin angleRad -- skip a * tileGlobalSize, seems like sizes are correct w/o
    viewCos = cos angleRad -- skip a * tileGlobalSize, seems like sizes are correct w/o
    focalX = x - (focalLength * viewCos)
    focalY = y + (focalLength * viewSin)
    focalXI :: Int
    focalXI = floor focalX
    focalYI :: Int
    focalYI = floor focalY
    focalTileX = focalXI `shiftR` tileToGlobalShift
    focalTileY = focalYI `shiftR` tileToGlobalShift
    viewTileX = ((floor x) `shiftR` tileToGlobalShift) :: Int
    viewTileY = ((floor y) `shiftR` tileToGlobalShift) :: Int
    xPartialDown = focalXI .&. (tileGlobalSize - 1)
    xPartialUp = tileGlobalSize - xPartialDown
    yPartialDown = focalYI .&. (tileGlobalSize - 1)
--    yPartialUp = tileGlobalSize - yPartialDown

--    xInitialIntercept = (viewXI + xPartialUp, 0) :: (Int, Int)
--    yInitialIntercept = (0, viewYI + yPartialUp) :: (Int, Int)

    -- This is for 0-89deg
    xStep = round (fromIntegral tileGlobalSize * tan (pi / 2 - angleRad)) :: Int
    yStep = - round (fromIntegral tileGlobalSize * tan angleRad) :: Int
    xTileStep = 1 :: Int
    yTileStep = -1 :: Int
    xPartial = xPartialUp
    yPartial = yPartialDown

    horIntercept = (focalXI + yPartial, focalTileY * tileGlobalSize) :: (Int, Int)
    verIntercept = ((focalTileX + xTileStep) * tileGlobalSize, focalYI - xPartial) :: (Int, Int)
    -- wasn't able to discern how it's done in wolf3d ASM, must be a more efficient way
    verDist = distSquared (focalXI, focalYI) verIntercept
    horDist = distSquared (focalXI, focalYI) horIntercept

-- couldn't get a power operator working :(
distSquared :: (Int, Int) -> (Int, Int) -> Int
distSquared (x1, y1) (x2, y2) = ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2))

verRayCheck :: WallMap -> (Int, Int) -> (Int, Int) -> Int -> Int -> Maybe Hit
verRayCheck wm hI (vIX, vIY) xStep yStep = case traceShow ("ver", vITileX, vITileY) hitting of
    Nothing -> traceShow " nothing" (horRayCheck wm hI (vIX + tileGlobalSize, vIY + yStep) xStep yStep)
    Just m  -> traceShow " mat" (Just (Hit {material=m}))
  where
    vITileX = (vIX - 1) `shiftR` tileToGlobalShift
    vITileY = (vIY - 1) `shiftR` tileToGlobalShift
    hitting = wm!!vITileX!!vITileY

horRayCheck :: WallMap -> (Int, Int) -> (Int, Int) -> Int -> Int ->Maybe Hit
horRayCheck wm (hIX, hIY) vI xStep yStep = case traceShow ("hor", hITileX, hITileY) hitting of
    Nothing -> traceShow " nothing" (verRayCheck wm (hIX + xStep, hIY - tileGlobalSize) vI xStep yStep)
    Just m  -> traceShow " mat" (Just (Hit {material=m}))
  where
    hITileX = (hIX - 1) `shiftR` tileToGlobalShift
    hITileY = (hIY - 1) `shiftR` tileToGlobalShift
    hitting = wm!!hITileX!!hITileY

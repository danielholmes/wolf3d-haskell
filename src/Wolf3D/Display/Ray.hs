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

data RayData = DiagonalRayData {horInterceptX :: Int
                                , horInterceptYTile :: Int
                                , verInterceptY :: Int
                                , verInterceptXTile :: Int
                                , xTileStep :: Int
                                , yTileStep :: Int
                                , xStep :: Int
                                , yStep :: Int} |
                HorizontalRayData {verInterceptY :: Int
                                  , verInterceptXTile :: Int
                                  , xTileStep :: Int} |
                VerticalRayData {horInterceptX :: Int
                                , horInterceptYTile :: Int
                                , yTileStep :: Int}
  deriving (Show)

castRay :: WallMap -> Vector2 -> SnappedRotation -> Maybe Hit
castRay [] _ _   = Nothing
castRay wm pos viewAngle = if startAtVer
                           then verRayCheck wm rd
                           else horRayCheck wm rd
  where (startAtVer, rd) = createRayData pos viewAngle

type StartAtVer = Bool

createRayData :: Vector2 -> SnappedRotation -> (StartAtVer, RayData)
createRayData (Vector2 x y) viewAngle
  | viewAngle == 0 || viewAngle == 180  = (True, HorizontalRayData verInterceptY1 verInterceptXTile1 xTileStep1)
  | viewAngle == 90 || viewAngle == 270 = (False, VerticalRayData horInterceptX1 horInterceptYTile1 yTileStep1)
  | otherwise                           = (xPartial < yPartial, DiagonalRayData horInterceptX1 horInterceptYTile1 verInterceptY1 verInterceptXTile1 xTileStep1 yTileStep1 xStep1 yStep1)
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
    xPartialDown = focalXI .&. (tileGlobalSize - 1)
    xPartialUp = tileGlobalSize - xPartialDown
    yPartialDown = focalYI .&. (tileGlobalSize - 1)
    yPartialUp = tileGlobalSize - yPartialDown

    -- This is for 0-89deg, 90-179
    -- Taken directly from source, find a better way
    is0To89 = viewAngle >= 0 && viewAngle < 90
    is90To179 = viewAngle >= 90 && viewAngle < 180
    is180To269 = viewAngle >= 180 && viewAngle < 270
    is270To359 = viewAngle >= 270 && viewAngle < 360
    xStep1 :: Int
    xStep1 = if is0To89
      then (round (fromIntegral tileGlobalSize * tan (pi / 2 - angleRad)))
      else if is90To179
       then (- round (fromIntegral tileGlobalSize * tan (angleRad - pi / 2)))
       else if is180To269
         then (- round (fromIntegral tileGlobalSize * tan (pi * 3 / 2 - angleRad)))
         else (round (fromIntegral tileGlobalSize * tan (angleRad - pi * 3 / 2)))
    yStep1 :: Int
    yStep1 = if is0To89
      then (-round (fromIntegral tileGlobalSize * tan angleRad))
      else if is90To179
        then (-round (fromIntegral tileGlobalSize * tan (pi - angleRad)))
        else if is180To269
          then (round (fromIntegral tileGlobalSize * tan (angleRad - pi)))
          else (round (fromIntegral tileGlobalSize * tan (2 * pi - angleRad)))

    xTileStep1 = if is0To89 || is270To359 then 1 else -1 :: Int
    yTileStep1 = if is180To269 || is270To359 then 1 else -1 :: Int
    xPartial = if is0To89 || is270To359 then xPartialUp else xPartialDown
    yPartial = if is180To269 || is270To359 then yPartialUp else yPartialDown

    horInterceptX1 = focalXI - (yTileStep1 * yPartial)
    horInterceptYTile1 = focalTileY + yTileStep1
    verInterceptY1 = focalYI - xPartial
    verInterceptXTile1 = focalTileX + xTileStep1

verRayCheck :: WallMap -> RayData -> Maybe Hit
verRayCheck _ (VerticalRayData {}) = error "Shouldn't be called"
verRayCheck wm d@DiagonalRayData {verInterceptY=vIY
                                  , verInterceptXTile=vIXTile
                                  , xTileStep=xTileS
                                  , yStep=dy} = case traceShow ("ver", vIXTile, vIYTile) hitting of
    Nothing -> traceShow " nothing" (horRayCheck wm d{verInterceptY=(vIY + dy), verInterceptXTile=(vIXTile + xTileS)})
    Just m  -> traceShow " mat" (Just (Hit {material=m}))
  where
    vIYTile = (vIY - 1) `shiftR` tileToGlobalShift
    hitting = wm!!vIXTile!!vIYTile
verRayCheck wm d@HorizontalRayData {verInterceptY=vIY
                                  , verInterceptXTile=vIXTile
                                  , xTileStep=xTileS} = case traceShow ("verHH", vIXTile, vIYTile) hitting of
    Nothing -> traceShow " nothing" (verRayCheck wm d{verInterceptXTile=(vIXTile + xTileS)})
    Just m  -> traceShow " mat" (Just (Hit {material=m}))
  where
    vIYTile = (vIY - 1) `shiftR` tileToGlobalShift
    hitting = wm!!vIXTile!!vIYTile

horRayCheck :: WallMap -> RayData -> Maybe Hit
horRayCheck _ (HorizontalRayData {}) = error "Shouldn't be called"
horRayCheck wm d@DiagonalRayData {horInterceptX=hIX
                                 , horInterceptYTile=hIYTile
                                 , yTileStep=dYTile
                                 , xStep=dX} = case traceShow ("hor", hIXTile, hIYTile) hitting of
    Nothing -> traceShow " nothing" (verRayCheck wm d{horInterceptX=(hIX + dX), horInterceptYTile=(hIYTile + dYTile)})
    Just m  -> traceShow " mat" (Just (Hit {material=m}))
  where
    hIXTile = (hIX - 1) `shiftR` tileToGlobalShift
    hitting = wm!!hIXTile!!hIYTile
horRayCheck wm d@VerticalRayData {horInterceptX=hIX
                                 , horInterceptYTile=hIYTile
                                 , yTileStep=dYTile} = case traceShow ("horVV", hIXTile, hIYTile) hitting of
    Nothing -> traceShow " nothing" (horRayCheck wm d{horInterceptYTile=(hIYTile + dYTile)})
    Just m  -> traceShow " mat" (Just (Hit {material=m}))
  where
    hIXTile = (hIX - 1) `shiftR` tileToGlobalShift
    hitting = wm!!hIXTile!!hIYTile

module Wolf3D.Display.Ray (
  castRay,
  HitDirection (Horizontal, Vertical),
  Hit (Hit, material, intercept, direction)
) where

import Data.Vector
import Wolf3D.Engine
import Wolf3D.Sim
import Wolf3D.Geom
import Data.Bits
import Debug.Trace

data HitDirection = Horizontal | Vertical
  deriving (Show, Eq)

data Hit = Hit {material :: WallMaterial, direction :: HitDirection, intercept :: (Int, Int)}
  deriving (Eq, Show)

-- Taken from original source, don't know exact meaning yet
focalLength :: Double
focalLength = 0x5700

--sinTable :: [Double]
--sinTable = map (\a -> (fromIntegral tileGlobalSize) * sin (a * deg2Rad)) [0..(360 - 1)]
--
--cosTable :: [Double]
--cosTable = map (\a -> (fromIntegral tileGlobalSize) * cos (a * deg2Rad)) [0..(360 - 1)]

-- TODO: checks for double hor/ver remove need for separate  hor/ver data. Trial it
data RayData = DiagonalRayData {horInterceptX :: Int
                                , horInterceptYTile :: Int
                                , verInterceptY :: Int
                                , verInterceptXTile :: Int
                                , xTileStep :: Int
                                , yTileStep :: Int
                                , xStep :: Int
                                , yStep :: Int
                                , horNextIntercept :: (Int, Int)
                                , verNextIntercept :: (Int, Int)} |
                HorizontalRayData {interceptY :: Int
                                  , interceptXTile :: Int
                                  , tileStep :: Int} |
                VerticalRayData {interceptX :: Int
                                , interceptYTile :: Int
                                , tileStep :: Int}
  deriving (Show)

castRay :: WallMap -> Vector2 -> SnappedRotation -> Maybe Hit
castRay [] _ _   = Nothing
castRay wm pos viewAngle = nextRayCheck wm focal rd
  where (focal, rd) = createRayData pos viewAngle

createRayData :: Vector2 -> SnappedRotation -> ((Int, Int), RayData)
createRayData (Vector2 x y) viewAngle
  | viewAngle == 0 || viewAngle == 180  = (focal
                                          , HorizontalRayData {interceptY=focalYI
                                                              , interceptXTile=verInterceptXTile1
                                                              , tileStep=xTileStep1})
  | viewAngle == 90 || viewAngle == 270 = (focal
                                          , VerticalRayData {interceptX=focalXI
                                                            , interceptYTile=horInterceptYTile1
                                                            , tileStep=yTileStep1})
  | viewAngle > 0 && viewAngle < 90     = let
                                            tanTheta = tan angleRad
                                            -- TODO: Shifting
                                            aX = focalXI + round (fromIntegral (focalYI - focalTileY * tileGlobalSize) * tanTheta)
                                            -- TODO: Use focalXI shift instead
                                            bYX = fromIntegral (focalXI - (focalTileX * tileGlobalSize))
                                            bY = focalYI - round (bYX / tanTheta)
                                          in
                                            (focal
                                            , DiagonalRayData {horNextIntercept=(aX, (horInterceptYTile1 + 1) * tileGlobalSize)
                                                             , verNextIntercept=((focalTileX + xTileStep1) * tileGlobalSize, bY)
                                                             , horInterceptX=aX
                                                             , horInterceptYTile=horInterceptYTile1
                                                             , verInterceptY=bY
                                                             , verInterceptXTile=focalTileX + xTileStep1
                                                             , xTileStep=xTileStep1
                                                             , yTileStep=yTileStep1
                                                             , xStep=(round (fromIntegral tileGlobalSize * tan (pi / 2 - angleRad)))
                                                             , yStep=(-round (fromIntegral tileGlobalSize * tan angleRad))})
  | viewAngle > 90 && viewAngle < 180   = let
                                            tanTheta = tan (angleRad - pi / 2)
                                            aY = focalTileY * tileGlobalSize
                                            aX = focalXI - round (fromIntegral (focalYI - aY) * tanTheta)
                                            -- TODO: Use focalXI shift instead
                                            bX = fromIntegral (focalXI - focalTileX * tileGlobalSize)
                                            bY = round (focalY - bX / tanTheta)
                                          in
                                            (focal
                                            , DiagonalRayData {horNextIntercept=(aX, (horInterceptYTile1 + 1) * tileGlobalSize)
                                                              , verNextIntercept=(focalTileX * tileGlobalSize, bY)
                                                              , horInterceptX=aX
                                                              , horInterceptYTile=horInterceptYTile1
                                                              , verInterceptY=bY
                                                              , verInterceptXTile=focalTileX + xTileStep1
                                                              , xTileStep=xTileStep1
                                                              , yTileStep=yTileStep1
                                                              , xStep=(-round (fromIntegral tileGlobalSize * tanTheta))
                                                              , yStep=(-round (fromIntegral tileGlobalSize / tanTheta))})
  | viewAngle > 180 && viewAngle < 270  = let
                                            tanTheta = tan (3/2 * pi - angleRad)
                                            aX = focalXI - round (fromIntegral (((focalTileY + yTileStep1) * tileGlobalSize) - focalYI) * tanTheta)
                                            -- TODO: Use focalXI shift instead
                                            bYX = fromIntegral (focalXI - (focalTileX * tileGlobalSize))
                                            bY = focalYI + round (bYX / tanTheta)
                                          in
                                            (focal
                                            , DiagonalRayData {horNextIntercept=(aX, horInterceptYTile1 * tileGlobalSize)
                                                              , verNextIntercept=(focalTileX * tileGlobalSize, bY)
                                                              , horInterceptX=aX
                                                              , horInterceptYTile=horInterceptYTile1
                                                              , verInterceptY=bY
                                                              , verInterceptXTile=focalTileX + xTileStep1
                                                              , xTileStep=xTileStep1
                                                              , yTileStep=yTileStep1
                                                              , xStep=(- round (fromIntegral tileGlobalSize * tanTheta))
                                                              , yStep=(round (fromIntegral tileGlobalSize * tan (angleRad - pi)))})
  | viewAngle > 270 && viewAngle < 360  = let
                                            tanTheta = tan (angleRad - 3/2 * pi)
                                            aX = focalXI + round (fromIntegral (((focalTileY + yTileStep1) * tileGlobalSize) - focalYI) * tanTheta)
                                            bY = focalYI + round (fromIntegral (((focalTileX + xTileStep1) * tileGlobalSize) - focalXI) / tanTheta)
                                          in
                                            (focal
                                            , DiagonalRayData {horNextIntercept=(aX, horInterceptYTile1 * tileGlobalSize)
                                                              , verNextIntercept=((focalTileX + xTileStep1) * tileGlobalSize, bY)
                                                              , horInterceptX=aX
                                                              , horInterceptYTile=horInterceptYTile1
                                                              , verInterceptY=bY
                                                              , verInterceptXTile=focalTileX + xTileStep1
                                                              , xTileStep=xTileStep1
                                                              , yTileStep=yTileStep1
                                                              , xStep=(round (fromIntegral tileGlobalSize * tanTheta))
                                                              , yStep=(round (fromIntegral tileGlobalSize * tan (2 * pi - angleRad)))})
  | otherwise                           = error "Angle must be >= 0 < 360"
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
    focal = (focalXI, focalYI)
    focalTileX = focalXI `shiftR` tileToGlobalShift
    focalTileY = focalYI `shiftR` tileToGlobalShift
--    xPartialDown = focalXI .&. (tileGlobalSize - 1)
--    xPartialUp = tileGlobalSize - xPartialDown
--    yPartialDown = focalYI .&. (tileGlobalSize - 1)
--    yPartialUp = tileGlobalSize - yPartialDown

    -- This is for 0-89deg, 90-179
    -- Taken directly from source, find a better way
    is0To89 = viewAngle >= 0 && viewAngle < 90
--    is90To179 = viewAngle >= 90 && viewAngle < 180
    is180To269 = viewAngle >= 180 && viewAngle < 270
    is270To359 = viewAngle >= 270 && viewAngle < 360

    xTileStep1 = if is0To89 || is270To359 then 1 else -1 :: Int
    yTileStep1 = if is180To269 || is270To359 then 1 else -1 :: Int

--    xPartial = if is0To89 || is270To359 then xPartialUp else xPartialDown
--    yPartial = if is180To269 || is270To359 then yPartialUp else yPartialDown

    horInterceptYTile1 = focalTileY + yTileStep1
    verInterceptXTile1 = focalTileX + xTileStep1

nextRayCheck :: WallMap -> (Int, Int) -> RayData -> Maybe Hit
nextRayCheck wm p@(_, y) nextD@DiagonalRayData{ horNextIntercept=(_, hY)
                                            , verNextIntercept=(vX, vY)}
  | nextHorYStep < nextVerYStep = traceShow debug (horRayCheck wm nextD)
  | otherwise                   = traceShow debug (verRayCheck wm nextD)
  where
    nextHorYStep = abs (hY - y)
    nextVerYStep = abs (vY - y)
    debug = ("   nextRayCheck", nextHorYStep, nextVerYStep, p, (vX, vY))
nextRayCheck wm _ d@(HorizontalRayData _ _ _) = verRayCheck wm d
nextRayCheck wm _ d@(VerticalRayData _ _ _) = horRayCheck wm d

verRayCheck :: WallMap -> RayData -> Maybe Hit
verRayCheck _ (VerticalRayData {}) = error "Shouldn't be called"
verRayCheck wm d@DiagonalRayData {verInterceptY=vIY
                                  , verInterceptXTile=vIXTile
                                  , xTileStep=xTileS
                                  , yStep=dy
                                  , verNextIntercept=currentIntercept@(x, y)} = case traceShow ("ver", currentIntercept, (vIXTile, vIYTile)) hitting of
    Nothing -> traceShow (" nothing", currentIntercept, vInterceptNext, dy) (nextRayCheck wm currentIntercept nextD)
    Just m  -> traceShow " mat" (Just (Hit {material=m, direction=Vertical, intercept=currentIntercept}))
  where
    vIYTile = (vIY - 1) `shiftR` tileToGlobalShift
    -- intXTile = if xTileS == -1 then vIXTile + 1 else vIXTile
    hitting = wm!!vIXTile!!vIYTile
    -- vIntercept = (intXTile * tileGlobalSize, vIY)
    
    vInterceptNext = (x + xTileS * tileGlobalSize, y + dy)
    nextD = d{verInterceptY=(vIY + dy)
              , verInterceptXTile=(vIXTile + xTileS)
              , verNextIntercept=vInterceptNext}
    
verRayCheck wm d@HorizontalRayData {interceptY=vIY
                                  , interceptXTile=vIXTile
                                  , tileStep=xTileS} = case traceShow ("verHH", vIXTile, vIYTile) hitting of
    Nothing -> traceShow " nothing" (verRayCheck wm d{interceptXTile=(vIXTile + xTileS)})
    Just m  -> traceShow " mat" (Just (Hit {material=m, direction=Vertical, intercept=vIntercept}))
  where
    vIYTile = (vIY - 1) `shiftR` tileToGlobalShift
    hitting = wm!!vIXTile!!vIYTile

    vInterceptX = vIXTile * tileGlobalSize
    vIntercept = (if xTileS == -1 then vInterceptX + tileGlobalSize else vInterceptX, vIY)

horRayCheck :: WallMap -> RayData -> Maybe Hit
horRayCheck _ (HorizontalRayData {}) = error "Shouldn't be called"
horRayCheck wm d@DiagonalRayData {horInterceptX=hIX
                                 , horInterceptYTile=hIYTile
                                 , yTileStep=dYTile
                                 , xStep=dX
                                 , horNextIntercept=currentIntercept@(x, y)} = case traceShow ("hor", currentIntercept, (hIXTile, hIYTile)) hitting of
    Nothing -> traceShow " nothing" (nextRayCheck wm currentIntercept nextD)
    Just m  -> traceShow " mat" (Just (Hit {material=m, direction=Horizontal, intercept=currentIntercept}))
  where
    hIXTile = (hIX - 1) `shiftR` tileToGlobalShift
    hitting = wm!!hIXTile!!hIYTile

    hInterceptNext = (x + dX, y + dYTile * tileGlobalSize)
    nextD = d{horInterceptX=(hIX + dX)
            , horInterceptYTile=(hIYTile + dYTile)
            , horNextIntercept=hInterceptNext}
horRayCheck wm d@VerticalRayData {interceptX=hIX
                                 , interceptYTile=hIYTile
                                 , tileStep=dYTile} = case traceShow ("horVV", hIXTile, hIYTile) hitting of
    Nothing -> traceShow " nothing" (horRayCheck wm d{interceptYTile=(hIYTile + dYTile)})
    Just m  -> traceShow " mat" (Just (Hit {material=m, direction=Horizontal, intercept=hIntercept}))
  where
    hIXTile = (hIX - 1) `shiftR` tileToGlobalShift
    hitting = wm!!hIXTile!!hIYTile

    hInterceptY = hIYTile * tileGlobalSize
    hIntercept = (hIX, if dYTile == -1 then hInterceptY + tileGlobalSize else hInterceptY)
